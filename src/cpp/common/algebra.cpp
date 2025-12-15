#include "common/algebra.hpp"
#include <boost/math/distributions/chi_squared.hpp>
#include <boost/math/distributions/normal.hpp>
#include <sstream>
#include <utility>
#include "architectureDocs.hpp"
#include "common/acsConfig.hpp"
#include "common/algebraTrace.hpp"
#include "common/common.hpp"
#include "common/constants.hpp"
#include "common/eigenIncluder.hpp"
#include "common/kalmanBlas.hpp"
#include "common/lapackWrapper.hpp"
#include "common/mongo.hpp"
#include "common/mongoWrite.hpp"
#include "common/trace.hpp"

using std::ostringstream;
using std::pair;

/** Kalman Filter.
 *
 * This software uses specialised kalman filter classes to perform filtering.
 * Using classes such as KFState, KFMeas, etc, prevents duplication of code, and ensures that many
 * edge cases are taken care of without the need for the developer to consider them explicitly.
 *
 * The basic workflow for using the filter is:
 * create filter object,
 * create a list of measurements (only adding entries for required states, using KFKeys to reference
 * the state element) combine measurements that in a list into a single matrix corresponding to the
 * new state, and filtering - filtering internally saves the states for RTS code, using a single
 * sequential file that has some headers added so that it can be traversed backwards
 *
 * The filter has some pre/post fit checks that remove measurements that are out of expected ranges,
 * and there are functions provided for setting, resetting, and getting values, noises, and
 * covariance values.
 *
 * KFKey objects are used to identify states. They may have a KF type, SatSys value, string (usually
 * used for receiver id), and number associated with them, and can be set and read from filter
 * objects as required.
 *
 * KFMeasEntry objects are used for an individual measurement, before being combined into KFMeas
 * objects that contain all of the measurements for a filter iteration.
 *
 * Internally, the data is stored in maps and Eigen matrices/vectors, but the accessors should be
 * used rather than the vectors themselves to ensure that states have been initialised and are in
 * the expected order.
 *
 * InitialState objects are created directly from yaml configurations, and contain the detailis
 * about state transitions, including process noise, which are automatically added to the filter
 * when a stateTransition() call is used, scaling any process noise according to the time gap since
 * the last stateTransition.
 *
 * $$ K = HPH^\intercal + R $$ fgdfg
 */
ParallelArchitecture Kalman_Filter__()
{
    DOCS_REFERENCE(Binary_Archive__);
}

// #pragma GCC optimize ("O0")

const KFKey KFState::oneKey = {.type = KF::ONE};

bool KFKey::operator==(const KFKey& b) const
{
    if (str != b.str)
        return false;
    if (Sat != b.Sat)
        return false;
    if (type != b.type)
        return false;
    if (num != b.num)
        return false;
    else
        return true;
}

bool KFKey::operator!=(const KFKey& b) const
{
    return !(*this == b);
}

bool KFKey::operator<(const KFKey& b) const
{
    if (str < b.str)
        return true;
    if (str > b.str)
        return false;

    if (Sat < b.Sat)
        return true;
    if (Sat > b.Sat)
        return false;

    if (type < b.type)
        return true;
    if (type > b.type)
        return false;

    if (num < b.num)
        return true;
    else
        return false;
}

/** Finds the position in the noise vector of particular noise elements.
 */
int KFMeas::getNoiseIndex(const KFKey& key)  ///< Key to search for in noise vector
    const
{
    auto index = noiseIndexMap.find(key);
    if (index == noiseIndexMap.end())
    {
        return -1;
    }

    return index->second;
}

/** Clears and initialises the state transition matrix to identity at the beginning of an epoch.
 * Also clears any noise that was being added for the initialisation of a new state.
 */
void KFState::initFilterEpoch(Trace& trace)
{
    initNoiseMap.clear();

    for (auto& [key1, mapp] : stateTransitionMap)
    {
        if (key1 == oneKey)
        {
            continue;
        }

        // remove initialisation elements for subsequent epochs
        mapp.erase(oneKey);
    }

    stateTransitionMap[oneKey][oneKey][0] = 1;

    // make a copy because iterators will be invalidated
    auto sigmaMaxMapCopy = sigmaMaxMap;

    // remove any states that have exceeded their max variances
    for (auto& [key, sigmaMax] : sigmaMaxMapCopy)
    {
        double sigma = 0;
        getKFSigma(key, sigma);

        if (sigma > sigmaMax)
        {
            trace << "\n"
                  << "Removing '" << key << "' due to large sigma";
            removeState(key);
        }
    }

    // make a copy because iterators will be invalidated
    auto outageLimitMapCopy = outageLimitMap;

    // remove any states that have exceeded their max outages
    for (auto& [key, outageLimit] : outageLimitMapCopy)
    {
        double outage = (time - key.estimatedTime).to_double();

        if (outage > outageLimit)
        {
            trace << "\n"
                  << "Removing '" << key << "' due to long outage";

            outageLimitMap.erase(key);

            removeState(key);
        }
    }
}

/** Finds the position in the KF state vector of particular states.
 */
int KFState::getKFIndex(const KFKey& key)  ///< Key to search for in state
    const
{
    auto index = kfIndexMap.find(key);
    if (index == kfIndexMap.end())
    {
        return -1;
    }
    return index->second;
}

vector<KFKey> KFState::decomposedStateKeys(const KFKey& composedKey) const
{
    auto it = pseudoStateMap.find(composedKey);
    if (it == pseudoStateMap.end())
    {
        return {composedKey};
    }

    auto& [dummy, pseudoMap] = *it;

    vector<KFKey> decomposedKeys;
    for (auto& [key, coeff] : pseudoMap)
    {
        decomposedKeys.push_back(key);
    }

    return decomposedKeys;
}

/** Retrieve values from pseudo-states.
 * Pseudo states are linear combinations of correlated states, and this function returns the value
 * of the states assuming that the correlations from the point of creation are still valid. It
 * returns the entirety of the state component for the primary state, with others returning 0. Later
 * linear combinations of these should return the correct value for the combined state.
 */
E_Source KFState::getPseudoValue(
    const KFKey& key,            ///< Key to search for in state
    double&      value,          ///< Output value
    double*      variance_ptr,   ///< Optional variance output
    double*      adjustment_ptr  ///< Optional adjustment output
) const
{
    lock_guard guard(kfStateMutex);

    auto it = pseudoParentMap.find(key);
    if (it == pseudoParentMap.end())
    {
        return E_Source::NONE;
    }

    // this is a linear combination, make assumptions and return the values

    auto& [dummy, parent] = *it;

    auto& pseudoMap = pseudoStateMap.at(parent);

    auto& [primary, coeff] = *pseudoMap.begin();

    if (key == primary)
    {
        // only the primary key returns values, assume all others 0

        double pseudoValue;
        double pseudoVariance;
        double pseudoAdjustment;
        getKFValue(parent, pseudoValue, &pseudoVariance, &pseudoAdjustment);

        double scalar = 1 / coeff;

        value = scalar * pseudoValue;
        if (variance_ptr)
            *variance_ptr = scalar * pseudoVariance * scalar;
        if (adjustment_ptr)
            *adjustment_ptr = scalar * pseudoAdjustment;

        return E_Source::PSEUDO;
    }

    value = 0;
    if (variance_ptr)
        *variance_ptr = 0;
    if (adjustment_ptr)
        *adjustment_ptr = 0;

    return E_Source::PSEUDO;
}

/** Returns the value and variance of a state within the kalman filter object
 */
E_Source KFState::getKFValue(
    const KFKey& key,             ///< Key to search for in state
    double&      value,           ///< Output value
    double*      variance_ptr,    ///< Optional variance output
    double*      adjustment_ptr,  ///< Optional adjustment output
    bool         allowAlternate   ///< Optional flag to disable alternate filter
) const
{
    auto a = kfIndexMap.find(key);
    if (a == kfIndexMap.end())
    {
        E_Source found = getPseudoValue(key, value, variance_ptr, adjustment_ptr);

        if (found != E_Source::NONE)
            return E_Source::PSEUDO;

        if (allowAlternate == false || alternate_ptr == nullptr)
        {
            return E_Source::NONE;
        }

        found = alternate_ptr->getKFValue(key, value, variance_ptr, adjustment_ptr);
        if (found != E_Source::NONE)
            return E_Source::REMOTE;

        return E_Source::NONE;
    }

    int index = a->second;
    if (index >= x.size())
    {
        return E_Source::NONE;
    }
    value = x(index);

    if (variance_ptr)
    {
        *variance_ptr = P(index, index);
    }

    if (adjustment_ptr)
    {
        *adjustment_ptr = dx(index);
    }

    return E_Source::KALMAN;
}

/** Returns the standard deviation of a state within the kalman filter object
 */
bool KFState::getKFSigma(
    const KFKey& key,   ///< Key to search for in state
    double&      sigma  ///< Output value
)
{
    auto a = kfIndexMap.find(key);
    if (a == kfIndexMap.end())
    {
        return false;
    }
    int index = a->second;
    if (index >= x.size())
    {
        return false;
    }

    sigma = sqrt(P(index, index));
    return true;
}

void KFState::setAccelerator(
    const KFKey&        element,
    const KFKey&        dotElement,
    const KFKey&        dotDotElement,
    const double        value,
    const InitialState& initialState
)
{
    addKFState(dotDotElement, initialState);

    // t^2 term
    stateTransitionMap[element][dotDotElement][2] = value;

    // t terms
    stateTransitionMap[dotElement][dotDotElement][1] = value;
}

/** Adds dynamics to a filter state by inserting off-diagonal, non-time dependent elements to
 * transition matrix
 */
void KFState::setKFTrans(
    const KFKey&        dest,         ///< Key to search for in state to change in transition
    const KFKey&        source,       ///< Key to search for in state as source
    const double        value,        ///< Input value
    const InitialState& initialState  ///< Initial state.
)
{
    lock_guard guard(kfStateMutex);

    addKFState(dest, initialState);

    auto& transition = stateTransitionMap[dest][source][0];

    transition += value;
}

/** Adds dynamics to a filter state by inserting off-diagonal, time dependent elements to transition
 * matrix
 */
void KFState::setKFTransRate(
    const KFKey&        integralKey,       ///< Key to search for in state to change in transition
    const KFKey&        rateKey,           ///< Key to search for in state as source
    const double        value,             ///< Input value
    const InitialState& initialRateState,  ///< Initial state for rate state.
    const InitialState&
        initialIntegralState  ///< Initial state for the thing that is modified by the rate
)
{
    lock_guard guard(kfStateMutex);

    addKFState(rateKey, initialRateState);
    addKFState(integralKey, initialIntegralState);

    stateTransitionMap[integralKey][rateKey][1] = value;
}

/** Remove a state from a kalman filter object.
 */
void KFState::removeState(
    const KFKey& kfKey,  ///< Key to search for in state
    bool         allowDeleteParent
)
{
    lock_guard guard(kfStateMutex);

    KFKey removeKey = kfKey;

    // for psuedo states this can get a bit recursive states are removed after they're combined, so
    // it needs to be prevent specifically in that case
    if (allowDeleteParent)
    {
        auto it = pseudoParentMap.find(kfKey);
        if (it != pseudoParentMap.end())
        {
            auto& [dummy, parent] = *it;

            removeKey = parent;

            traceTrivialTrace(
                "Removing '%s' because of '%s'",
                ((string)removeKey).c_str(),
                ((string)kfKey).c_str()
            );
        }
    }

    traceTrivialTrace("Removing '%s'", ((string)removeKey).c_str());

    stateTransitionMap.erase(removeKey);
    sigmaMaxMap.erase(removeKey);
    procNoiseMap.erase(removeKey);
    gaussMarkovTauMap.erase(removeKey);
    gaussMarkovMuMap.erase(removeKey);
    exponentialNoiseMap.erase(removeKey);
    errorCountMap.erase(removeKey);

    // do pseudo state removal after state transition is complete,
    // outage limits should stay with the non-pseudo states, and shouldnt be deleted with the
    // non-pseudo
    //  outageLimitMap.			erase(removeKey);
}

/** Tries to add a state to the filter object.
 *  If it does not exist, it adds it to a list of states to be added.
 *  Call consolidateKFState() to apply the list to the filter object
 */
bool KFState::addKFState(
    const KFKey&        kfKey,        ///< The key to add to the state
    const InitialState& initialState  ///< The initial conditions to add to the state
)
{
    lock_guard guard(kfStateMutex);

    auto iter = stateTransitionMap.find(kfKey);
    if (iter != stateTransitionMap.end())
    {
        // is an existing state, just update values
        if (initialState.Q != 0)
        {
            procNoiseMap[kfKey] = initialState.Q;
        }
        if (initialState.mu != 0)
        {
            gaussMarkovMuMap[kfKey] = initialState.mu;
        }
        if (initialState.tau != 0)
        {
            gaussMarkovTauMap[kfKey] = initialState.tau;
        }
        if (initialState.sigmaMax != 0)
        {
            sigmaMaxMap[kfKey] = initialState.sigmaMax;
        }
        if (initialState.outageLimit != 0)
        {
            outageLimitMap[kfKey] = initialState.outageLimit;
        }

        return false;
    }

    auto pseudoIt = pseudoParentMap.find(kfKey);
    if (pseudoIt != pseudoParentMap.end())
    {
        // this is an existing pseudostate, dont re-add a real state
        return false;
    }

    // this should be a new state, add to the state transition matrix to create a new state.

    // check if it exists in the state though, identity STM causes double ups if its reinitialised
    // too quickly
    double currentX = 0;
    auto   it       = kfIndexMap.find(kfKey);
    if (it != kfIndexMap.end())
    {
        auto& [dummy, index] = *it;
        currentX             = x[index];
    }

    stateTransitionMap[kfKey][kfKey][0]  = 1;
    stateTransitionMap[kfKey][oneKey][0] = initialState.x - currentX;
    if (initialState.P)
        initNoiseMap[kfKey] = initialState.P;
    if (initialState.Q)
        procNoiseMap[kfKey] = initialState.Q;
    if (initialState.mu)
        gaussMarkovMuMap[kfKey] = initialState.mu;
    if (initialState.tau)
        gaussMarkovTauMap[kfKey] = initialState.tau;
    if (initialState.sigmaMax)
        sigmaMaxMap[kfKey] = initialState.sigmaMax;
    if (initialState.outageLimit)
        outageLimitMap[kfKey] = initialState.outageLimit;

    if (initialState.P < 0)
    {
        // will be an uninitialised variable, do a least squares solution
        lsqRequired = true;
    }

    return true;
}

/** Create a pseudo state that represents the linear combination of two or more perfectly correlated
 * states. The configuration of the states that are combined are added according to the coefficients
 * of correlation, which may not be appropriate for things like process noise or max sigmas, and
 * should be configured accordingly.
 */
bool KFState::addPseudoState(const KFKey& kfKey, const map<KFKey, double>& coeffMap)
{
    lock_guard guard(kfStateMutex);

    InitialState init;
    init.P = 0;

    for (auto& [key, coeff] : coeffMap)
    {
        auto& parentKey = pseudoParentMap[key];

        if (parentKey.type != KF::NONE && parentKey != kfKey)
        {
            BOOST_LOG_TRIVIAL(warning) << "Pseudo state added with multiple parents. " << key
                                       << " has parents '" << parentKey << "' and '" << kfKey;

            throw true;
        }
    }

    for (auto& [key, coeff] : coeffMap)
    {
        pseudoParentMap[key] = kfKey;

        // get initial values from other states
        init.x += coeff * stateTransitionMap[key][oneKey][0];
        init.P += coeff * initNoiseMap[key] * coeff;

        // expect these to not be in the maps, use `at` rather than adding surplus entries or
        // dealing with iterators
        try
        {
            init.sigmaMax += coeff * sigmaMaxMap.at(key);
        }
        catch (...)
        {
        }
        try
        {
            init.Q += coeff * procNoiseMap.at(key);
        }
        catch (...)
        {
        }
        try
        {
            init.tau += gaussMarkovTauMap.at(key);
        }
        catch (...)
        {
        }

        removeState(key, false);
    }

    pseudoStateMap[kfKey] = coeffMap;

    bool stateCreated = addKFState(kfKey, init);

    return stateCreated;
}

void KFState::setExponentialNoise(const KFKey& kfKey, const Exponential exponential)
{
    lock_guard guard(kfStateMutex);

    exponentialNoiseMap[kfKey] = exponential;
}

/** Add process noise and dynamics to filter object manually. BEWARE!
 * Not recommended for ordinary use, likely to break things. Dont touch unless you really know what
 * you're doing. Hint - you dont really know what you're doing
 */
void KFState::manualStateTransition(Trace& trace, GTime newTime, MatrixXd& F, MatrixXd& Q0)
{
    if (newTime != GTime::noTime())
    {
        time = newTime;
    }

    // output the state transition matrix to a trace file (used by RTS smoother)
    if (rts_basename.empty() == false)
    {
        TransitionMatrixObject transitionMatrixObject = F;

        spitFilterToFile(
            transitionMatrixObject,
            E_SerialObject::TRANSITION_MATRIX,
            rts_basename + FORWARD_SUFFIX,
            acsConfig.pppOpts.queue_rts_outputs
        );
    }

    // compute the updated states and permutation and covariance matrices
    VectorXd Fx = F * x;
    if (simulate_filter_only == false)
    {
        dx = Fx - x;
        x  = (Fx).eval();
        P  = (F * P * F.transpose() + Q0).eval();
    }

    initFilterEpoch(trace);
}

/** Add process noise and dynamics to filter object according to time gap.
 * This will also sort states according to their kfKey as a result of the way the state transition
 * matrix is generated.
 */
void KFState::stateTransition(
    Trace&    trace,    ///< Trace file for output
    GTime     newTime,  ///< Time of update for process noise and dynamics (s)
    MatrixXd* stm_ptr   ///< Optional pointer to output state transition matrix
)
{
    double tgap = 0;
    if (newTime != GTime::noTime() && time != GTime::noTime())
    {
        tgap = (newTime - time).to_double();
    }

    if (newTime != GTime::noTime())
    {
        time = newTime;
    }

    int newStateCount = stateTransitionMap.size();
    if (newStateCount == 0)
    {
        std::cout << "THIS IS WEIRD" << "\n";
        return;
    }

    // Initialise and populate a state transition and Z transition matrix
    SparseMatrix<double> F = SparseMatrix<double>(newStateCount, x.rows());

    // add transitions for any states (usually close to identity)
    int             row = 0;
    map<KFKey, int> newKFIndexMap;
    for (auto& [newStateKey, newStateMap] : stateTransitionMap)
    {
        newKFIndexMap[newStateKey] = row;

        for (auto& [sourceStateKey, values] : newStateMap)
        {
            int sourceIndex = getKFIndex(sourceStateKey);

            if ((sourceIndex < 0) || (sourceIndex >= F.cols()))
            {
                continue;
            }

            for (auto& [tExp, value] : values)
            {
                double tau = -1;

                auto gmIter = gaussMarkovTauMap.find(sourceStateKey);
                if (gmIter != gaussMarkovTauMap.end())
                {
                    auto& [dummy, sourceTau] = *gmIter;

                    tau = sourceTau;
                }

                double scalar = 1;

                if (tau < 0)
                {
                    // Random Walk model (special case for First Order Gauss Markov model when tau
                    // == inf)

                    for (int i = 0; i < tExp; i++)
                    {
                        scalar *= tgap / (i + 1);
                    }

                    // 				F(row, sourceIndex) = value * scalar;
                    F.coeffRef(row, sourceIndex) += value * scalar;

                    continue;
                }

                // First Order Gauss Markov model, Ref: Carpenter and Lee (2008) - A Stable Clock
                // Error Model Using Coupled First- and Second-Order Gauss-Markov Processes -
                // https://ntrs.nasa.gov/api/citations/20080044877/downloads/20080044877.pdf

                double tempTerm = 1;
                scalar          = exp(-tgap / tau);

                for (int i = 0; i < tExp; i++)
                {
                    scalar = tau * (tempTerm - scalar);  // recursive formula derived according to
                                                         // Ref: Carpenter and Lee (2008)
                    tempTerm *= tgap / (i + 1);
                }

                double transition = value * scalar;

                // 			F(row, sourceIndex) = transition;
                F.coeffRef(row, sourceIndex) += transition;

                // Add state transitions to ONE element, to allow for tiedown to average value mu
                // derived from integrating and distributing terms for v = (v0 - mu) * exp(-t/tau) +
                // mu; tempTerm calculated above appears to be same as required for these terms too,
                // (at least for tExp = 0,1)

                auto muIter = gaussMarkovMuMap.find(sourceStateKey);
                if (muIter != gaussMarkovMuMap.end())
                {
                    auto& [dummy2, mu] = *muIter;

                    // 				F(row, 0) = mu * (tempTerm - transition);
                    F.coeffRef(row, 0) += mu * (tempTerm - transition);
                }
            }
        }

        row++;
    }

    // scale and add process noise
    MatrixXd Q0 = MatrixXd::Zero(newStateCount, newStateCount);
    tgap        = fabs(tgap);

    // add noise as 'process noise' as the method of initialising a state's variance
    for (auto& [kfKey, value] : initNoiseMap)
    {
        auto iter = newKFIndexMap.find(kfKey);
        if (iter == newKFIndexMap.end())
        {
            // 			std::cout << kfKey << " broke" << "\n";
            continue;
        }
        int index = iter->second;

        if ((index < 0) || (index >= Q0.rows()))
        {
            continue;
        }

        Q0(index, index) = value;
    }

    // add time dependent exponential process noise)
    if (tgap)
        for (auto& [dest, exponential] : exponentialNoiseMap)
        {
            auto destIter = newKFIndexMap.find(dest);
            if (destIter == newKFIndexMap.end())
            {
                std::cout << dest << " broke" << "\n";
                continue;
            }

            auto& expNoise = exponential.value;

            // if (expNoise > 0.01)
            // {
            trace << "\n"
                  << "Adding " << expNoise << " (per second) to process noise for " << dest << "\n";
            // }

            int destIndex = destIter->second;

            if ((destIndex < 0) || (destIndex >= Q0.rows()))
            {
                continue;
            }

            Q0(destIndex, destIndex) += expNoise * tgap;
        }

    // shrink time dependent exponential process noise
    if (tgap)
        for (auto& [dest, exponential] : exponentialNoiseMap)
        {
            auto& expNoise = exponential.value;
            auto& expTau   = exponential.tau;

            // shrink the exponential process noise the next time around
            if (expTau)
                expNoise *= exp(-tgap / expTau);
            else
                expNoise = 0;
        }

    // add time dependent process noise
    if (tgap)
        for (auto& [dest, map] : stateTransitionMap)
            for (auto& [source, vals] : map)
                for (auto& [tExp, val] : vals)
                {
                    auto initIter = initNoiseMap.find(dest);
                    if (initIter != initNoiseMap.end())
                    {
                        // this was initialised this epoch
                        double init = initIter->second;

                        if (init == 0)
                        {
                            // this was initialised with no noise, do lsq, dont add process noise
                            continue;
                        }
                    }

                    auto destIter = newKFIndexMap.find(dest);
                    if (destIter == newKFIndexMap.end())
                    {
                        std::cout << dest << " broke" << "\n";
                        continue;
                    }

                    int destIndex = destIter->second;

                    if ((destIndex < 0) || (destIndex >= Q0.rows()))
                    {
                        continue;
                    }

                    auto sourceIter = newKFIndexMap.find(source);
                    if (sourceIter == newKFIndexMap.end())
                    {
                        std::cout << dest << " broKe" << "\n";
                        continue;
                    }

                    int sourceIndex = sourceIter->second;

                    if ((sourceIndex < 0) || (sourceIndex >= Q0.rows()))
                    {
                        continue;
                    }

                    auto iter2 = procNoiseMap.find(source);
                    if (iter2 == procNoiseMap.end())
                    {
                        // 			std::cout << dest << " brOke" << "\n";
                        continue;
                    }

                    auto [dummy, sourceProcessNoise] = *iter2;

                    auto gmIter = gaussMarkovTauMap.find(source);
                    if (gmIter != gaussMarkovTauMap.end())
                    {
                        auto& [dummy, tau] = *gmIter;

                        if (tau < 0)
                        {
                            // Random Walk model (special case for First Order Gauss Markov model
                            // when tau == inf)

                            if (tExp == 0)
                            {
                                Q0(destIndex, destIndex) += sourceProcessNoise / 1 * tgap;
                            }
                            // 				else if	(tExp == 1)	{	Q0(destIndex,	destIndex) +=
                            // sourceProcessNoise / 3
                            // * tgap * tgap * tgap; 								Q0(sourceIndex,
                            // destIndex) += sourceProcessNoise / 2	* tgap * tgap;
                            // Q0(destIndex, sourceIndex) += sourceProcessNoise / 2	* tgap * tgap;
                            // 									}
                            // 				else if (tExp == 2)	{	Q0(destIndex,	destIndex) +=
                            // sourceProcessNoise / 20	* tgap * tgap * tgap * tgap * tgap;}
                        }
                        else
                        {
                            // First Order Gauss Markov model, Ref: Carpenter and Lee (2008) - A
                            // Stable Clock Error Model Using Coupled First- and Second-Order
                            // Gauss-Markov Processes -
                            // https://ntrs.nasa.gov/api/citations/20080044877/downloads/20080044877.pdf

                            if (tExp == 0)
                            {
                                Q0(destIndex, destIndex) +=
                                    sourceProcessNoise / 2 * tau * (1 - exp(-2 * tgap / tau));
                            }
                            else if (tExp == 1)
                            {
                                Q0(destIndex, destIndex) +=
                                    sourceProcessNoise / 2 * tau * tau *
                                    (+2 * tgap  // one tau from front tau3 distributed to prevent
                                                // divide by zero
                                     - 4 * tau * (1 - exp(-1 * tgap / tau)) +
                                     1 * tau * (1 - exp(-2 * tgap / tau))
                                    );  // correct formula re-derived according
                                        // to Ref: Carpenter and Lee (2008)
                                // Q0(sourceIndex, destIndex) +=
                                // sourceProcessNoise / 2
                                // * tau * tau * (1-exp(-tgap/tau)) * (1-exp(-tgap/tau));
                                // Q0(destIndex, sourceIndex) += sourceProcessNoise / 2	* tau * tau
                                // * (1-exp(-tgap/tau))
                                // * (1-exp(-tgap/tau));
                            }
                            else if (tExp == 2)
                            {
                                std::cout << "FOGM model is not applied to acceleration term at "
                                             "the moment"
                                          << "\n";
                            }
                        }
                    }
                    else
                    {
                        std::cout << "Tau value not found in filter: " << source << "\n";
                        continue;
                    }
                }

    // output the state transition matrix to a trace file (used by RTS smoother)
    if (rts_basename.empty() == false)
    {
        TransitionMatrixObject transitionMatrixObject = F;

        spitFilterToFile(
            transitionMatrixObject,
            E_SerialObject::TRANSITION_MATRIX,
            rts_basename + FORWARD_SUFFIX,
            acsConfig.pppOpts.queue_rts_outputs
        );
    }

    if (stm_ptr)
    {
        *stm_ptr = F;
    }

    // 	std::cout << "x" << "\n" << x << "\n";
    // compute the updated states and permutation and covariance matrices
    VectorXd Fx = F * x;
    if (F.rows() == F.cols())
    {
        dx = Fx - x;
    }
    else
    {
        dx = VectorXd::Zero(F.rows());
    }

    {
        x = (Fx).eval();
    }
    if (simulate_filter_only)
    {
        Q0.setZero();
    }
    {
        P = (F * P * F.transpose() + Q0).eval();
    }
    // std::cout << "F" << "\n" << MatrixXd(F).format(heavyFmt) << "\n";
    // 	std::cout << "x1" << "\n" << MatrixXd(x).transpose().format(HeavyFmt) << "\n";
    // 	std::cout << "Q0" << "\n" << Q0 << "\n";
    // 	std::cout << "P" << "\n" << P << "\n";

    // replace the index map with the updated version that corresponds to the updated state
    kfIndexMap = std::move(newKFIndexMap);

    // remove any details about pseudo states that are no longer in the state vector
    // do it here because it causes issues if maps dont exist in other places before state
    // transitions
    for (auto pseudoIt = pseudoStateMap.begin(); pseudoIt != pseudoStateMap.end();)
    {
        auto& [parent, childrenMap] = *pseudoIt;

        auto it = kfIndexMap.find(parent);

        if (it == kfIndexMap.end())
        {
            // not in state any more, remove the pseudos

            // first erase the children pointing to this parent from the other map
            for (auto& [child, coeff] : childrenMap)
            {
                pseudoParentMap.erase(child);
            }

            // and erase it from this map pointing to them
            pseudoIt = pseudoStateMap.erase(pseudoIt);
        }
        else
        {
            pseudoIt++;
        }
    }

    initFilterEpoch(trace);
}

/** Compare variances of measurements and estimated parameters to detect unreasonable values
 * Ref: to be added
 */
void KFState::leastSquareSigmaChecks(
    RejectCallbackDetails& callbackDetails,
    MatrixXd&              Pp,         ///< Post-fit covariance of parameters
    KFStatistics&          statistics  ///< Test statistics
)
{
    auto& kfMeas = callbackDetails.kfMeas;
    auto& trace  = callbackDetails.trace;

    auto& VV = kfMeas.VV;
    auto& R  = kfMeas.R;
    auto& H  = kfMeas.H;

    ArrayXd measRatios      = ArrayXd::Zero(H.rows());
    ArrayXd measNumerator   = ArrayXd::Zero(H.rows());
    ArrayXd measDenominator = ArrayXd::Zero(H.rows());

    if (lsqOpts.sigma_check)
    {
        // use 'array' for component-wise calculations
        measNumerator   = VV.array();
        measDenominator = R.diagonal().array();
    }
    else if (lsqOpts.omega_test)
    {
        MatrixXd HPH_ = H * Pp * H.transpose();

        // use 'array' for component-wise calculations
        measNumerator   = VV.array();
        measDenominator = (R.diagonal() - HPH_.diagonal()).array();
    }

    measRatios = measNumerator / measDenominator.sqrt();
    measRatios = measRatios.isFinite().select(
        measRatios,
        0
    );  // set ratio to 0 if corresponding variance is 0, e.g. ONE state, clk rate states

    kfMeas.postfitRatios = measRatios;

    statistics.sumOfSquares = measRatios.square().sum();
    statistics.averageRatio = measRatios.mean();

    Eigen::ArrayXd::Index measIndex;

    double maxMeasRatio = measRatios.abs().maxCoeff(&measIndex);

    // if any are outside the expected values, flag an error
    if (maxMeasRatio > lsqOpts.meas_sigma_threshold)
    {
        trace << "\n"
              << time << "\tLARGE MEAS    ERROR OF : " << maxMeasRatio << "\tAT " << measIndex
              << " :\t" << kfMeas.obsKeys[measIndex];

        callbackDetails.measIndex = measIndex;
    }
}

/** Compare variances of measurements and pre-filtered states to detect unreasonable values
 * Ref: Wang et al. (1997) - On Quality Control in Hydrographic GPS Surveying
 * &  Wieser et al. (2004) - Failure Scenarios to be Considered with Kinematic High Precision
 * Relative GNSS Positioning
 * - http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.573.9628&rep=rep1&type=pdf
 */
void KFState::preFitSigmaChecks(
    RejectCallbackDetails& callbackDetails,
    KFStatistics&          statistics,  ///< Test statistics
    int                    begX,        ///< Index of first state element to process
    int                    numX,        ///< Number of states elements to process
    int                    begH,        ///< Index of first measurement to process
    int                    numH         ///< Number of measurements to process
)
{
    auto& trace  = callbackDetails.trace;
    auto& kfMeas = callbackDetails.kfMeas;

    auto V = kfMeas.V.segment(begH, numH);
    auto R = kfMeas.R.block(begH, begH, numH, numH);
    auto H = kfMeas.H.block(begH, begX, numH, numX);
    auto P = this->P.block(begX, begX, numX, numX);

    ArrayXd  measRatios  = ArrayXd::Zero(numH);
    ArrayXd  stateRatios = ArrayXd::Zero(numX);
    MatrixXd HPH_        = H * P * H.transpose();

    if (prefitOpts.sigma_check)
    {
        // use 'array' for component-wise calculations
        auto measVariations = V.array();
        auto measVariances  = (HPH_.diagonal() + R.diagonal()).array();

        measRatios = measVariations / measVariances.sqrt();
    }
    else if (prefitOpts.omega_test)  // Eugene: will be gone
    {
        MatrixXd Qinv   = (HPH_ + R).inverse();
        MatrixXd H_Qinv = H.transpose() * Qinv;

        // use 'array' for component-wise calculations
        auto measNumerator  = (Qinv * V).array();
        auto stateNumerator = (H_Qinv * V).array();

        auto measDenominator  = Qinv.diagonal().array();
        auto stateDenominator = (H_Qinv * H).diagonal().array();

        measRatios  = measNumerator / measDenominator.sqrt();
        stateRatios = stateNumerator / stateDenominator.sqrt();
    }

    // Eugene: do abs() here
    measRatios = measRatios.isFinite().select(
        measRatios,
        0
    );  // set ratio to 0 if corresponding variance is 0, e.g. ONE state, clk rate states
    stateRatios = stateRatios.isFinite().select(stateRatios, 0);

    kfMeas.prefitRatios.segment(begH, numH) = measRatios;
    this->prefitRatios.segment(begX, numX)  = stateRatios;

    statistics.sumOfSquares = measRatios.square().sum();
    statistics.averageRatio = measRatios.mean();

    Eigen::ArrayXd::Index stateIndex;
    Eigen::ArrayXd::Index measIndex;

    double maxStateRatio = stateRatios.abs().maxCoeff(&stateIndex);
    double maxMeasRatio  = measRatios.abs().maxCoeff(&measIndex);

    int stateChunkIndex = stateIndex + begX;
    int measChunkIndex  = measIndex + begH;

    auto it = kfIndexMap.begin();
    std::advance(it, stateChunkIndex);

    auto& [stateKey, dummy] = *it;

    // if any are outside the expected values, flag an error
    if (maxStateRatio * sqrt(0.95) > maxMeasRatio &&
        maxStateRatio > prefitOpts.state_sigma_threshold)
    {
        trace << "\n"
              << time << "\tLARGE STATE   ERROR OF : " << maxStateRatio << "\tAT "
              << stateChunkIndex << " :\t" << stateKey;
        trace << "\n"
              << time << "\tLargest meas  error is : " << maxMeasRatio << "\tAT " << measChunkIndex
              << " :\t" << kfMeas.obsKeys[measChunkIndex] << "\n";

        auto mask = (H.col(stateIndex).array() != 0);  // Mask out referencing measurements, i.e.
                                                       // non-zero values in column stateIndex of H
        measRatios.array() *=
            mask.cast<double>();  // Set measRatios of non-referencing measurements to 0

        maxMeasRatio   = measRatios.abs().maxCoeff(&measIndex);
        measChunkIndex = measIndex + begH;

        trace << "\n"
              << time << "\tLargest  ref meas error is: " << maxMeasRatio << "\tAT "
              << measChunkIndex << " :\t" << kfMeas.obsKeys[measChunkIndex] << "\n";

        callbackDetails.kfKey      = stateKey;
        callbackDetails.stateIndex = stateChunkIndex;
        callbackDetails.measIndex  = measChunkIndex;

        measRatios          = (measRatios == 0).select(INFINITY, measRatios);
        double minMeasRatio = measRatios.abs().minCoeff(&measIndex);
        measChunkIndex      = measIndex + begH;

        trace << "\n"
              << time << "\tSmallest ref meas error is: " << minMeasRatio << "\tAT "
              << measChunkIndex << " :\t" << kfMeas.obsKeys[measChunkIndex] << "\n";
    }
    else if (maxMeasRatio > prefitOpts.meas_sigma_threshold)
    {
        trace << "\n"
              << time << "\tLARGE MEAS    ERROR OF : " << maxMeasRatio << "\tAT " << measChunkIndex
              << " :\t" << kfMeas.obsKeys[measChunkIndex];
        trace << "\n"
              << time << "\tLargest state error is : " << maxStateRatio << "\tAT "
              << stateChunkIndex << " :\t" << stateKey << "\n";

        callbackDetails.measIndex = measChunkIndex;
    }
}

void outputResiduals(
    Trace&  trace,      ///< Trace file to output to
    KFMeas& kfMeas,     ///< Measurements, noise, and design matrix
    string  suffix,     ///< Suffix to use in header
    int     iteration,  ///< Number of iterations prior to this check
    int     begH,       ///< Index of first measurement to process
    int     numH        ///< Number of measurements to process
)
{
    tracepdeex(0, trace, "\n");

    string name = "RESIDUALS";
    name += suffix;

    Block block(trace, name);

    tracepdeex(
        0,
        trace,
        "#\t%2s\t%22s\t%12s\t%4s\t%4s\t%7s\t%17s\t%17s\t%16s",
        "It",
        "Time",
        "Type",
        "Sat",
        "Str",
        "Code",
        "Prefit Res",
        "Postfit Res",
        "Meas Sigma"
    );
    tracepdeex(5, trace, "\t%16s", "Prefit Ratio");
    tracepdeex(5, trace, "\t%16s", "Postfit Ratio");
    tracepdeex(2, trace, "\t%s", "Comments");
    tracepdeex(0, trace, "\n");

    int endH;
    if (numH < 0)
        endH = kfMeas.obsKeys.size();
    else
        endH = begH + numH;

    for (int i = begH; i < endH; i++)
    {
        char preResStr[20];
        char postResStr[20];
        char sigmaStr[20];
        char preRatioStr[20];
        char postRatioStr[20];

        double V = kfMeas.V(i);

        if (V == 0 || (fabs(V) > 0.0001 && fabs(V) < 1e7))
            snprintf(preResStr, sizeof(preResStr), "%17.8f", V);
        else
            snprintf(preResStr, sizeof(preResStr), "%17.4e", V);

        double VV = kfMeas.VV(i);

        if (VV == 0 || (fabs(VV) > 0.0001 && fabs(VV) < 1e7))
            snprintf(postResStr, sizeof(postResStr), "%17.8f", VV);
        else
            snprintf(postResStr, sizeof(postResStr), "%17.4e", VV);

        double sigma = sqrt(kfMeas.R(i, i));

        if (sigma == 0 || (fabs(sigma) > 0.0001 && fabs(sigma) < 1e7))
            snprintf(sigmaStr, sizeof(sigmaStr), "%16.8f", sigma);
        else
            snprintf(sigmaStr, sizeof(sigmaStr), "%16.4e", sigma);

        double preRatio = 0;
        if (i < kfMeas.prefitRatios.rows())
            preRatio = kfMeas.prefitRatios(i);

        if (preRatio == 0 || (fabs(preRatio) > 0.001 && fabs(preRatio) < 1e7))
            snprintf(preRatioStr, sizeof(preRatioStr), "%16.7f", preRatio);
        else
            snprintf(preRatioStr, sizeof(preRatioStr), "%16.3e", preRatio);

        double postRatio = 0;
        if (i < kfMeas.postfitRatios.rows())
            postRatio = kfMeas.postfitRatios(i);

        if (postRatio == 0 || (fabs(postRatio) > 0.001 && fabs(postRatio) < 1e7))
            snprintf(postRatioStr, sizeof(postRatioStr), "%16.7f", postRatio);
        else
            snprintf(postRatioStr, sizeof(postRatioStr), "%16.3e", postRatio);

        tracepdeex(
            0,
            trace,
            "%%\t%2d\t%22s\t%30s\t%17.8f\t%17.8f\t%16s",
            iteration,
            kfMeas.time.to_string(2).c_str(),
            ((string)kfMeas.obsKeys[i]).c_str(),
            preResStr,
            postResStr,
            sigmaStr
        );
        tracepdeex(5, trace, "\t%16s", preRatioStr);
        tracepdeex(5, trace, "\t%16s", postRatioStr);
        tracepdeex(2, trace, "\t%s", kfMeas.obsKeys[i].comment.c_str());
        tracepdeex(0, trace, "\n");
    }
}

/** Compare variances of measurements and filtered states to detect unreasonable values
 */
void KFState::postFitSigmaChecks(
    RejectCallbackDetails& callbackDetails,
    VectorXd&              dx,          ///< The state innovations from filtering
    MatrixXd&              Qinv,        ///< Inverse of innovation covariance matrix
    MatrixXd&              QinvH,       ///< Qinv*H matrix for omega test
    KFStatistics&          statistics,  ///< Test statistics
    int                    begX,        ///< Index of first state element to process
    int                    numX,        ///< Number of state elements to process
    int                    begH,        ///< Index of first measurement to process
    int                    numH         ///< Number of measurements to process
)
{
    auto& trace  = callbackDetails.trace;
    auto& kfMeas = callbackDetails.kfMeas;

    auto V  = kfMeas.V.segment(begH, numH);
    auto VV = kfMeas.VV.segment(begH, numH);
    auto R  = kfMeas.R.block(begH, begH, numH, numH);
    auto H  = kfMeas.H.block(begH, begX, numH, numX);
    auto P  = this->P.block(begX, begX, numX, numX);

    ArrayXd measRatios  = ArrayXd::Zero(numH);
    ArrayXd stateRatios = ArrayXd::Zero(numX);

    if (postfitOpts.sigma_check)
    {
        // use 'array' for component-wise calculations
        auto measVariations  = VV.array();
        auto stateVariations = dx.segment(begX, numX).array();

        auto measVariances  = R.diagonal().array();
        auto stateVariances = P.diagonal().array();

        measRatios  = measVariations / measVariances.sqrt();
        stateRatios = stateVariations / stateVariances.sqrt();
    }
    else if (postfitOpts.omega_test)
    {
        // use 'array' for component-wise calculations
        auto measNumerator  = (Qinv * V).array();
        auto stateNumerator = (QinvH.transpose() * V).array();

        auto measDenominator  = Qinv.diagonal().array();
        auto stateDenominator = (QinvH.transpose() * H).diagonal().array();

        measRatios  = measNumerator / measDenominator.sqrt();
        stateRatios = stateNumerator / stateDenominator.sqrt();
    }

    // Eugene: do abs() here
    measRatios = measRatios.isFinite().select(
        measRatios,
        0
    );  // set ratio to 0 if corresponding variance is 0, e.g. ONE state, clk rate states
    stateRatios = stateRatios.isFinite().select(stateRatios, 0);

    kfMeas.postfitRatios.segment(begH, numH) = measRatios;
    this->postfitRatios.segment(begX, numX)  = stateRatios;

    statistics.sumOfSquares = measRatios.square().sum();
    statistics.averageRatio = measRatios.mean();

    Eigen::ArrayXd::Index stateIndex;
    Eigen::ArrayXd::Index measIndex;

    double maxStateRatio = stateRatios.abs().maxCoeff(&stateIndex);
    double maxMeasRatio  = measRatios.abs().maxCoeff(&measIndex);

    int stateChunkIndex = stateIndex + begX;
    int measChunkIndex  = measIndex + begH;

    auto it = kfIndexMap.begin();
    std::advance(it, stateChunkIndex);

    auto& [stateKey, dummy] = *it;

    // if any are outside the expected values, flag an error
    if (maxStateRatio * sqrt(0.95) > maxMeasRatio &&
        maxStateRatio > postfitOpts.state_sigma_threshold)
    {
        trace << "\n"
              << time << "\tLARGE STATE   ERROR OF : " << maxStateRatio << "\tAT "
              << stateChunkIndex << " :\t" << stateKey;
        trace << "\n"
              << time << "\tLargest meas  error is : " << maxMeasRatio << "\tAT " << measChunkIndex
              << " :\t" << kfMeas.obsKeys[measChunkIndex] << "\n";

        auto mask = (H.col(stateIndex).array() != 0);  // Mask out referencing measurements, i.e.
                                                       // non-zero values in column stateIndex of H
        measRatios.array() *=
            mask.cast<double>();  // Set measRatios of non-referencing measurements to 0

        maxMeasRatio   = measRatios.abs().maxCoeff(&measIndex);
        measChunkIndex = measIndex + begH;

        trace << "\n"
              << time << "\tLargest  ref meas error is: " << maxMeasRatio << "\tAT "
              << measChunkIndex << " :\t" << kfMeas.obsKeys[measChunkIndex] << "\n";

        callbackDetails.kfKey      = stateKey;
        callbackDetails.stateIndex = stateChunkIndex;
        callbackDetails.measIndex  = measChunkIndex;

        measRatios          = (measRatios == 0).select(INFINITY, measRatios);
        double minMeasRatio = measRatios.abs().minCoeff(&measIndex);
        measChunkIndex      = measIndex + begH;

        trace << "\n"
              << time << "\tSmallest ref meas error is: " << minMeasRatio << "\tAT "
              << measChunkIndex << " :\t" << kfMeas.obsKeys[measChunkIndex] << "\n";
    }
    else if (maxMeasRatio > postfitOpts.meas_sigma_threshold)
    {
        trace << "\n"
              << time << "\tLARGE MEAS    ERROR OF : " << maxMeasRatio << "\tAT " << measChunkIndex
              << " :\t" << kfMeas.obsKeys[measChunkIndex];
        trace << "\n"
              << time << "\tLargest state error is : " << maxStateRatio << "\tAT "
              << stateChunkIndex << " :\t" << stateKey << "\n";

        callbackDetails.measIndex = measChunkIndex;
    }
}

/** Compute Chi-square increment based on the change of fitting solution
 */
double KFState::stateChiSquare(
    Trace&    trace,  ///< Trace file to output to
    MatrixXd& Pp,     ///< Post-update covariance of states
    VectorXd& dx,     ///< The state innovations from filtering
    int       begX,   ///< Index of first state element to process
    int       numX,   ///< Number of states elements to process
    int       begH,   ///< Index of first measurement to process
    int       numH    ///< Number of measurements to process
)
{
    if (begX == 0)  // exclude the One state
    {
        begX = 1;
        numX -= 1;
    }

    auto w = dx.segment(begX, numX);
    auto P = this->P.block(begX, begX, numX, numX);
    // MatrixXd	dP	= this->P.block(begX, begX, numX, numX) - Pp.block(begX, begX, numX, numX);
    // //Ref: Li et al. (2020)
    // - Robust Kalman Filtering Based on Chi-square Increment and Its Application -
    // https://www.mdpi.com/2072-4292/12/4/732/pdf

    double chiSq = w.transpose() * P.inverse() * w;
    // double		chiSq = w.transpose() * dP.inverse() * w;	//Ref: Li et al. (2020) - Robust
    // Kalman Filtering Based on Chi-square Increment and Its Application -
    // https://www.mdpi.com/2072-4292/12/4/732/pdf numerical instability problem exists for
    // dP.inverse()

    trace << "\n"
          << "DOING STATE CHI-SQUARE TEST:";
    // for (int i = 0; i < numX; i++)	trace << "dx: " 	<< w(i) << "\tdP: "	<< dP(i, i) << "\n";

    return chiSq;
}

/** Compute Chi-square increment based on post-fit residuals
 */
double KFState::measChiSquare(
    Trace&    trace,   ///< Trace file to output to
    KFMeas&   kfMeas,  ///< Measurements, noise, and design matrix
    VectorXd& dx,      ///< The state innovations from filtering
    int       begX,    ///< Index of first state element to process
    int       numX,    ///< Number of states elements to process
    int       begH,    ///< Index of first measurement to process
    int       numH     ///< Number of measurements to process
)
{
    auto w  = dx.segment(begX, numX);
    auto H  = kfMeas.H.block(begH, begX, numH, numX);
    auto VV = kfMeas.VV.segment(begH, numH);
    auto R  = kfMeas.R.block(begH, begH, numH, numH);

    double chiSq = (VV.array().square() / R.diagonal().array()).sum();

    trace << "\n"
          << "DOING MEASUREMENT CHI-SQUARE TEST:";
    // for (int i = 0; i < numH; i++)	trace << "v(+): "	<< VV(i) << "\tR: "		<< R(i, i) <<
    // "\n";

    return chiSq;
}

/** Compute Chi-square increment based on pre-fit residuals (innovations)
 */
double KFState::innovChiSquare(
    Trace&  trace,   ///< Trace to output to
    KFMeas& kfMeas,  ///< Measurements, noise, and design matrix
    int     begX,    ///< Index of first state element to process
    int     numX,    ///< Number of states elements to process
    int     begH,    ///< Index of first measurement to process
    int     numH     ///< Number of measurements to process
)
{
    auto     H = kfMeas.H.block(begH, begX, numH, numX);
    auto     V = kfMeas.V.segment(begH, numH);
    auto     R = kfMeas.R.block(begH, begH, numH, numH);
    auto     P = this->P.block(begX, begX, numX, numX);
    MatrixXd Q = R + H * P * H.transpose();

    double chiSq = V.transpose() * Q.inverse() * V;

    trace << "\n"
          << "DOING INNOVATION CHI-SQUARE TEST:";
    // for (int i = 0; i < numH; i++)	trace << "v(-): "	<< v(i) << "\tS: "		<< Q(i, i) <<
    // "\n";

    return chiSq;
}

/** Kalman filter.
 */
bool KFState::kFilter(
    Trace&    trace,   ///< Trace to output to
    KFMeas&   kfMeas,  ///< Measurements, noise, and design matrices
    VectorXd& xp,      ///< Post-update state vector
    MatrixXd& Pp,      ///< Post-update covariance of states
    VectorXd&
              dx,    ///< Post-update state innovation	 // Eugene: change name to avoid interference
    MatrixXd& Qinv,  ///< Inverse of innovation covariance matrix
    MatrixXd& QinvH,  ///< Qinv*H matrix for omega test
    int       begX,   ///< Index of first state element to process
    int       numX,   ///< Number of state elements to process
    int       begH,   ///< Index of first measurement to process
    int       numH    ///< Number of measurements to process
)
{
    auto& R      = kfMeas.R;
    auto& V      = kfMeas.V;
    auto& H      = kfMeas.H;
    auto& H_star = kfMeas.H_star;

    auto noise = kfMeas.uncorrelatedNoise.asDiagonal();  // todo Eugene: check chunking indices

    // Get pointers to block data (no copying!)
    const double* H_ptr = H.data() + begH + begX * H.rows();  // H block starting point
    const double* P_ptr = P.data() + begX + begX * P.rows();  // P block starting point
    const double* R_ptr = R.data() + begH + begH * R.rows();  // R block starting point
    const double* V_ptr = V.data() + begH;                    // V segment starting point

    int ldH = H.rows();                                       // Leading dimension of full H matrix
    int ldP = P.rows();                                       // Leading dimension of full P matrix
    int ldR = R.rows();                                       // Leading dimension of full R matrix

    MatrixXd I         = MatrixXd::Identity(numH, numH);
    auto     subH_star = H_star.middleRows(begH, numH);
    MatrixXd HRH_star  = subH_star * noise * subH_star.transpose();

    // Compute HP = H * P using BLAS directly on blocks (no copy!)
    MatrixXd HP(numH, numX);
    LapackWrapper::dgemm(
        LapackWrapper::CblasColMajor,
        LapackWrapper::CblasNoTrans,
        LapackWrapper::CblasNoTrans,
        numH,
        numX,
        numX,
        1.0,
        H_ptr,
        ldH,
        P_ptr,
        ldP,
        0.0,
        HP.data(),
        numH
    );

    // Compute Q = HP * H' + R using BLAS directly
    MatrixXd Q(numH, numH);
    // First: Q = R (copy R block)
    for (int j = 0; j < numH; j++)
    {
        LapackWrapper::dcopy(numH, R_ptr + j * ldR, 1, Q.data() + j * numH, 1);
    }
    // Then: Q = HP * H' + Q
    LapackWrapper::dgemm(
        LapackWrapper::CblasColMajor,
        LapackWrapper::CblasNoTrans,
        LapackWrapper::CblasTrans,
        numH,
        numH,
        numX,
        1.0,
        HP.data(),
        numH,
        H_ptr,
        ldH,
        1.0,
        Q.data(),
        numH
    );

    MatrixXd K;
    MatrixXd HRHQ_star;

    // Sequential solver fallback chain using separate factorization and solve
    // This allows us to factorize Q once and reuse it for multiple solves
    // Order: dpotrf/dpotrs -> dsytrf/dsytrs -> dgetrf/dgetrs

    int              info;
    std::vector<int> ipiv(numH);
    MatrixXd         Q_work      = Q;  // Working copy for factorization
    char             uplo        = 'U';
    int              solver_used = 0;  // 1=Cholesky, 2=LDLT, 3=LU

    // Try 1: Cholesky factorization (dpotrf) - fastest, for symmetric positive definite
    info = LapackWrapper::dpotrf(LapackWrapper::COL_MAJOR, uplo, numH, Q_work.data(), numH);

    if (info == 0)
    {
        solver_used = 1;
    }
    else
    {
        BOOST_LOG_TRIVIAL(warning) << "dpotrf (Cholesky factorization) failed with info = " << info
                                   << ", trying dsytrf (symmetric indefinite)";

        // Cholesky failed, restore Q and try symmetric indefinite
        Q_work = Q;

        // Try 2: Symmetric indefinite factorization (dsytrf)
        info = LapackWrapper::dsytrf(
            LapackWrapper::COL_MAJOR,
            uplo,
            numH,
            Q_work.data(),
            numH,
            ipiv.data()
        );

        if (info == 0)
        {
            solver_used = 2;
        }
        else
        {
            BOOST_LOG_TRIVIAL(warning)
                << "dsytrf (symmetric indefinite factorization) failed with info = " << info
                << ", trying dgetrf (general LU)";

            // Both symmetric solvers failed, try general LU
            Q_work = Q;

            // Try 3: General LU factorization (dgetrf)
            info = LapackWrapper::dgetrf(
                LapackWrapper::COL_MAJOR,
                numH,
                numH,
                Q_work.data(),
                numH,
                ipiv.data()
            );

            if (info == 0)
            {
                solver_used = 3;
            }
            else
            {
                // All factorizations failed
                BOOST_LOG_TRIVIAL(error)
                    << "dgetrf (general LU factorization) failed with info = " << info
                    << " - all factorization methods exhausted";

                xp = x;
                Pp = P;
                dx = VectorXd::Zero(xp.rows());

                trace << "\n" << "Kalman Filter Error - Matrix Factorization Failed";
                trace << "\n" << "Q: " << "\n" << Q;
                trace << "\n" << "H block size: " << numH << "x" << numX;
                trace << "\n" << "R block size: " << numH << "x" << numH;
                trace << "\n" << "P block size: " << numX << "x" << numX;

                return false;
            }
        }
    }

    // Now Q_work contains the factorization, solve multiple systems using the same factorization

    // System 1: Compute Kalman gain: Solve Q * K' = HP for K'
    MatrixXd KT = HP;  // Will be overwritten with solution

    if (solver_used == 1)
    {
        // Cholesky solve
        info = LapackWrapper::dpotrs(
            LapackWrapper::COL_MAJOR,
            uplo,
            numH,
            numX,
            Q_work.data(),
            numH,
            KT.data(),
            numH
        );
    }
    else if (solver_used == 2)
    {
        // Symmetric indefinite solve
        info = LapackWrapper::dsytrs(
            LapackWrapper::COL_MAJOR,
            uplo,
            numH,
            numX,
            Q_work.data(),
            numH,
            ipiv.data(),
            KT.data(),
            numH
        );
    }
    else  // solver_used == 3
    {
        // General LU solve
        info = LapackWrapper::dgetrs(
            LapackWrapper::COL_MAJOR,
            'N',
            numH,
            numX,
            Q_work.data(),
            numH,
            ipiv.data(),
            KT.data(),
            numH
        );
    }

    if (info != 0)
    {
        BOOST_LOG_TRIVIAL(error) << "Solve failed for Kalman gain with info = " << info;

        xp = x;
        Pp = P;
        dx = VectorXd::Zero(xp.rows());
        return false;
    }

    // Transpose to get K = (K')'
    K = KT.transpose();

    // System 3: Compute HRHQ_star for advanced postfits (do this FIRST - no copy needed)
    // Reuse the same factorization from Q_work directly
    if (advanced_postfits)
    {
        HRHQ_star = HRH_star;

        if (solver_used == 1)
        {
            // Cholesky solve: Q * HRHQ_star = HRH_star
            info = LapackWrapper::dpotrs(
                LapackWrapper::COL_MAJOR,
                uplo,
                numH,
                numH,
                Q_work.data(),
                numH,
                HRHQ_star.data(),
                numH
            );
        }
        else if (solver_used == 2)
        {
            // Symmetric indefinite solve: Q * HRHQ_star = HRH_star
            info = LapackWrapper::dsytrs(
                LapackWrapper::COL_MAJOR,
                uplo,
                numH,
                numH,
                Q_work.data(),
                numH,
                ipiv.data(),
                HRHQ_star.data(),
                numH
            );
        }
        else  // solver_used == 3
        {
            // General LU solve: Q * HRHQ_star = HRH_star
            info = LapackWrapper::dgetrs(
                LapackWrapper::COL_MAJOR,
                'N',
                numH,
                numH,
                Q_work.data(),
                numH,
                ipiv.data(),
                HRHQ_star.data(),
                numH
            );
        }

        if (info == 0)
        {
            HRHQ_star.transposeInPlace();
        }
        else
        {
            BOOST_LOG_TRIVIAL(warning)
                << "Failed to compute HRHQ_star for advanced postfits (info = " << info << ")";
        }
    }

    // System 2: Compute Qinv if needed for omega test (do this LAST)
    // Use direct matrix inversion (dpotri/dsytri/dgetri) working directly on Q_work
    // Since System 3 is already done, Q_work is no longer needed
    if (postfitOpts.omega_test)
    {
        // Work directly on Q_work (will be overwritten with inverse, no copy needed!)
        if (solver_used == 1)
        {
            // Cholesky: Compute inverse directly from L*L' factorization
            info = LapackWrapper::dpotri(LapackWrapper::COL_MAJOR, uplo, numH, Q_work.data(), numH);

            if (info == 0)
            {
                // dpotri only fills upper triangle (uplo='U'), copy to lower using pointer access
                double* Q_ptr = Q_work.data();
                for (int j = 0; j < numH; j++)
                {
                    for (int i = j + 1; i < numH; i++)
                    {
                        Q_ptr[j * numH + i] =
                            Q_ptr[i * numH + j];  // Column-major: copy upper to lower
                    }
                }
            }
        }
        else if (solver_used == 2)
        {
            // Symmetric indefinite: Compute inverse directly from LDLT factorization
            info = LapackWrapper::dsytri(
                LapackWrapper::COL_MAJOR,
                uplo,
                numH,
                Q_work.data(),
                numH,
                ipiv.data()
            );

            if (info == 0)
            {
                // dsytri only fills upper triangle (uplo='U'), copy to lower using pointer access
                double* Q_ptr = Q_work.data();
                for (int j = 0; j < numH; j++)
                {
                    for (int i = j + 1; i < numH; i++)
                    {
                        Q_ptr[j * numH + i] =
                            Q_ptr[i * numH + j];  // Column-major: copy upper to lower
                    }
                }
            }
        }
        else  // solver_used == 3
        {
            // General LU: Compute inverse directly from PLU factorization
            info = LapackWrapper::dgetri(
                LapackWrapper::COL_MAJOR,
                numH,
                Q_work.data(),
                numH,
                ipiv.data()
            );
            // dgetri fills the full matrix, no symmetrization needed
        }

        if (info == 0)
        {
            // Quick validation - sample diagonal elements for NaN/Inf (faster than checking all n²)
            bool      is_valid      = true;
            const int sample_stride = std::max(1, numH / 10);  // Sample ~10 elements
            for (int i = 0; i < numH; i += sample_stride)
            {
                if (!std::isfinite(Q_work(i, i)))  // Check diagonal
                {
                    is_valid = false;
                    BOOST_LOG_TRIVIAL(error)
                        << "Matrix inversion produced invalid diagonal value (NaN/Inf) at row " << i
                        << ", solver_used=" << solver_used;
                    break;
                }
            }

            if (is_valid)
            {
                // Assign inverted Q_work to Qinv (move semantics, no copy!)
                Qinv = std::move(Q_work);

                // Compute QinvH = Qinv * H
                QinvH.resize(numH, numX);
                LapackWrapper::dgemm(
                    LapackWrapper::CblasColMajor,
                    LapackWrapper::CblasNoTrans,
                    LapackWrapper::CblasNoTrans,
                    numH,
                    numX,
                    numH,
                    1.0,
                    Qinv.data(),
                    numH,
                    H_ptr,
                    ldH,
                    0.0,
                    QinvH.data(),
                    numH
                );
            }
            else
            {
                // Fallback: compute Qinv by solving if inversion produced invalid values
                BOOST_LOG_TRIVIAL(warning)
                    << "Falling back to solving Q*Qinv=I due to invalid inversion result";

                Qinv = MatrixXd::Identity(numH, numH);

                // Re-copy factorization (it was overwritten)
                LapackWrapper::dcopy(numH * numH, Q_work.data(), 1, Qinv.data(), 1);

                if (solver_used == 1)
                {
                    LapackWrapper::dpotrs(
                        LapackWrapper::COL_MAJOR,
                        uplo,
                        numH,
                        numH,
                        Q_work.data(),
                        numH,
                        Qinv.data(),
                        numH
                    );
                }
                else if (solver_used == 2)
                {
                    LapackWrapper::dsytrs(
                        LapackWrapper::COL_MAJOR,
                        uplo,
                        numH,
                        numH,
                        Q_work.data(),
                        numH,
                        ipiv.data(),
                        Qinv.data(),
                        numH
                    );
                }
                else
                {
                    LapackWrapper::dgetrs(
                        LapackWrapper::COL_MAJOR,
                        'N',
                        numH,
                        numH,
                        Q_work.data(),
                        numH,
                        ipiv.data(),
                        Qinv.data(),
                        numH
                    );
                }

                QinvH.resize(numH, numX);
                LapackWrapper::dgemm(
                    LapackWrapper::CblasColMajor,
                    LapackWrapper::CblasNoTrans,
                    LapackWrapper::CblasNoTrans,
                    numH,
                    numX,
                    numH,
                    1.0,
                    Qinv.data(),
                    numH,
                    H_ptr,
                    ldH,
                    0.0,
                    QinvH.data(),
                    numH
                );
            }
        }
        else
        {
            BOOST_LOG_TRIVIAL(error) << "Matrix inversion failed with info = " << info
                                     << ", solver_used=" << solver_used;
        }
    }

    if (advanced_postfits)
    {
        kfMeas.VV.segment(begH, numH) = HRHQ_star * VectorXd::Map(V_ptr, numH);
    }

    // Use BLAS-optimized state update: dx = K * v
    LapackWrapper::dgemv(
        LapackWrapper::CblasColMajor,
        LapackWrapper::CblasNoTrans,
        numX,
        numH,
        1.0,
        K.data(),
        numX,
        V_ptr,
        1,
        0.0,
        dx.data() + begX,
        1
    );

    // xp = x + dx
    LapackWrapper::dcopy(numX, x.data() + begX, 1, xp.data() + begX, 1);
    LapackWrapper::daxpy(numX, 1.0, dx.data() + begX, 1, xp.data() + begX, 1);

    // 	trace << "\n" << "H "	<< "\n" << subH;
    // 	trace << "\n" << "Hp "	<< "\n" << HP;
    // 	trace << "\n" << "Q "	<< "\n" << Q;
    // 	trace << "\n" << "K "	<< "\n" << K;
    // 	trace << "\n" << "x "	<< "\n" << x. segment(begX, numX);
    // 	trace << "\n" << "dx"	<< "\n" << dx.segment(begX, numX);
    // 	trace << "\n" << "xp"	<< "\n" << xp.segment(begX, numX);

    // Use BLAS-optimized covariance update
    MatrixXd subPp(numX, numX);
    if (joseph_stabilisation)
    {
        // Joseph form: Pp = (I-K*H)*P*(I-K*H)' + K*R*K'

        // Step 1: IKH = I - K*H
        MatrixXd IKH = MatrixXd::Identity(numX, numX);
        LapackWrapper::dgemm(
            LapackWrapper::CblasColMajor,
            LapackWrapper::CblasNoTrans,
            LapackWrapper::CblasNoTrans,
            numX,
            numX,
            numH,
            -1.0,
            K.data(),
            numX,
            H_ptr,
            ldH,
            1.0,
            IKH.data(),
            numX
        );

        // Step 2: temp = IKH * P
        MatrixXd temp(numX, numX);
        double*  P_block = const_cast<double*>(P_ptr);
        LapackWrapper::dgemm(
            LapackWrapper::CblasColMajor,
            LapackWrapper::CblasNoTrans,
            LapackWrapper::CblasNoTrans,
            numX,
            numX,
            numX,
            1.0,
            IKH.data(),
            numX,
            P_block,
            ldP,
            0.0,
            temp.data(),
            numX
        );

        // Step 3: subPp = temp * IKH'
        LapackWrapper::dgemm(
            LapackWrapper::CblasColMajor,
            LapackWrapper::CblasNoTrans,
            LapackWrapper::CblasTrans,
            numX,
            numX,
            numX,
            1.0,
            temp.data(),
            numX,
            IKH.data(),
            numX,
            0.0,
            subPp.data(),
            numX
        );

        // Step 4: temp2 = K * R
        MatrixXd temp2(numX, numH);
        double*  R_block = const_cast<double*>(R_ptr);
        LapackWrapper::dgemm(
            LapackWrapper::CblasColMajor,
            LapackWrapper::CblasNoTrans,
            LapackWrapper::CblasNoTrans,
            numX,
            numH,
            numH,
            1.0,
            K.data(),
            numX,
            R_block,
            ldR,
            0.0,
            temp2.data(),
            numX
        );

        // Step 5: subPp += temp2 * K'
        LapackWrapper::dgemm(
            LapackWrapper::CblasColMajor,
            LapackWrapper::CblasNoTrans,
            LapackWrapper::CblasTrans,
            numX,
            numX,
            numH,
            1.0,
            temp2.data(),
            numX,
            K.data(),
            numX,
            1.0,
            subPp.data(),
            numX
        );
    }
    else
    {
        // Standard form: Pp = P - K*HP

        // Copy P block into subPp
        double* P_block = const_cast<double*>(P_ptr);
        for (int j = 0; j < numX; j++)
        {
            LapackWrapper::dcopy(numX, P_block + j * ldP, 1, subPp.data() + j * numX, 1);
        }

        // Compute KHP = K * HP
        MatrixXd KHP(numX, numX);
        LapackWrapper::dgemm(
            LapackWrapper::CblasColMajor,
            LapackWrapper::CblasNoTrans,
            LapackWrapper::CblasNoTrans,
            numX,
            numX,
            numH,
            1.0,
            K.data(),
            numX,
            HP.data(),
            numH,
            0.0,
            KHP.data(),
            numX
        );

        // subPp = subPp - KHP
        LapackWrapper::daxpy(numX * numX, -1.0, KHP.data(), 1, subPp.data(), 1);
    }

    // Symmetrize for numerical stability
    for (int i = 0; i < numX; i++)
    {
        for (int j = i + 1; j < numX; j++)
        {
            double avg  = (subPp(i, j) + subPp(j, i)) / 2.0;
            subPp(i, j) = avg;
            subPp(j, i) = avg;
        }
    }

    Pp.block(begX, begX, numX, numX) = (subPp + subPp.transpose()).eval() / 2;

    bool error = xp.segment(begX, numX).array().isNaN().any();
    if (error)
    {
        std::cout << "\n"
                  << "xp:" << "\n"
                  << xp.segment(begX, numX);
        std::cout << "\n"
                  << "x :" << "\n"
                  << x.segment(begX, numX);
        std::cout << "\n"
                  << "P block:" << "\n"
                  << P.block(begX, begX, numX, numX);
        std::cout << "\n"
                  << "R block:" << "\n"
                  << R.block(begH, begH, numH, numH);
        std::cout << "\n"
                  << "K :" << "\n"
                  << K;
        std::cout << "\n"
                  << "V segment:" << "\n"
                  << V.segment(begH, numH);
        std::cout << "\n";
        std::cout << "NAN found. Exiting...";
        std::cout << "\n";

        exit(0);
    }

    return true;
}

/** Least squares estimator.
 */
bool KFState::leastSquare(
    Trace&    trace,   ///< Trace to output to
    KFMeas&   kfMeas,  ///< Measurements, noise, and design matrices
    VectorXd& xp,      ///< Post-fit parameter vector
    MatrixXd& Pp       ///< Post-fit covariance of parameters
)
{
    // invert measurement noise matrix to get a weight matrix
    ArrayXd weights = 1 / kfMeas.R.diagonal().array();  // Eugene to check if R is diagonal
    weights =
        weights.isFinite().select(weights, 0);  // Set weight to 0 if corresponding variance is 0
    kfMeas.W = weights.matrix();

    auto& H = kfMeas.H;
    auto& V = kfMeas.V;

    int numX = H.cols();
    int numH = H.rows();

    if (numX == 0 || numH == 0)
    {
        trace << "\n"
              << "EMPTY DESIGN MATRIX DURING LEAST SQUARES";
        return false;
    }

    if (numH < numX)
    {
        trace << "\n"
              << "Insufficient measurements for least squares " << numH << " < " << numX;
        return false;
    }

    // calculate least squares solution
    MatrixXd I   = MatrixXd::Identity(numX, numX);
    MatrixXd W   = kfMeas.W.asDiagonal();
    MatrixXd H_W = H.transpose() * W;
    MatrixXd N   = H_W * H;

    bool repeat = true;
    while (repeat)
    {
        switch (lsq_inverter)
        {
            default:
            {
                BOOST_LOG_TRIVIAL(warning) << "Least squares inverter type " << lsq_inverter
                                           << " not supported, reverting";

                lsq_inverter = E_Inverter::LDLT;
                continue;
            }
            case E_Inverter::LDLT:
            {
                auto           NN = N.triangularView<Eigen::Upper>().transpose();
                LDLT<MatrixXd> solver;
                if ((solver.compute(NN), solver.info() != Eigen::ComputationInfo::Success) ||
                    (Pp = solver.solve(I), solver.info() != Eigen::ComputationInfo::Success))
                {
                    BOOST_LOG_TRIVIAL(error)
                        << "Failed to solve normal equation, see trace file for matrices";

                    trace << "\n"
                          << "Least Squares Error";
                    trace << "\n"
                          << "N: " << "\n"
                          << N;
                    trace << "\n"
                          << "H: " << "\n"
                          << H;
                    trace << "\n"
                          << "W: " << "\n"
                          << W;

                    return false;
                }

                break;
            }
            case E_Inverter::LLT:
            {
                auto          NN = N.triangularView<Eigen::Upper>().transpose();
                LLT<MatrixXd> solver;
                if ((solver.compute(NN), solver.info() != Eigen::ComputationInfo::Success) ||
                    (Pp = solver.solve(I), solver.info() != Eigen::ComputationInfo::Success))
                {
                    BOOST_LOG_TRIVIAL(warning) << "Normal matrix not invertible with LLT "
                                                  "inverter, trying LDLT inverter instead";

                    lsq_inverter = E_Inverter::LDLT;
                    continue;
                }

                break;
            }
            case E_Inverter::INV:
            {
                Pp = N.inverse();

                if (Pp.array().isNaN().any() || Pp.array().isInf().any())
                {
                    BOOST_LOG_TRIVIAL(warning) << "Normal matrix not invertible with INV "
                                                  "inverter, trying LDLT inverter instead";

                    lsq_inverter = E_Inverter::LDLT;
                    continue;
                }

                break;
            }
        }

        repeat = false;
    }

    xp = Pp * H_W * V;

    // 	std::cout << "N : " << "\n" << N;
    bool error = xp.array().isNaN().any();
    if (error)
    {
        std::cout << "\n"
                  << "xp:" << "\n"
                  << xp << "\n";
        std::cout << "\n"
                  << "P :" << "\n"
                  << P << "\n";
        std::cout << "\n"
                  << "W :" << "\n"
                  << W << "\n";
        std::cout << "\n"
                  << "H :" << "\n"
                  << H << "\n";
        std::cout << "\n";
        std::cout << "NAN found. Exiting....";
        std::cout << "\n";

        exit(-1);
    }

    return true;
}

/** Perform chi squared quality control.
 */
void KFState::chiQC(
    Trace&  trace,  ///< Trace to output to
    KFMeas& kfMeas  ///< Measurements, noise, and design matrix
)
{
    auto& VV = kfMeas.VV;
    auto  W  = kfMeas.W.asDiagonal();

    dof = VV.rows() - (x.rows() - 1);  // ignore KF::ONE element -> -1
    if (dof < 1)
    {
        trace << "Error with Chi-square test: dof=" << dof;

        chiQCPass  = false;
        chi2       = 0;
        chi2PerDof = NAN;
        qc         = 0;

        return;
    }

    chi2       = VV.transpose() * W * VV;
    chi2PerDof = chi2 / dof;

    boost::math::normal normDist;

    double alpha = cdf(complement(normDist, chiSquareTest.sigma_threshold)) * 2;  // two-tailed

    boost::math::chi_squared chiSqDist(dof);

    qc = quantile(complement(chiSqDist, alpha));

    /* chi-square validation */
    if (chi2 > qc)
    {
        chiQCPass = false;
        return;
    }

    chiQCPass = true;
    return;
}

/** Combine a list of KFMeasEntrys into a single KFMeas object for used in the filter
 */
KFMeas::KFMeas(
    KFState&         kfState,         ///< Filter state to correspond to
    KFMeasEntryList& kfEntryList,     ///< List of input measurements as lists of entries
    GTime            measTime,        ///< Time to use for measurements and hence state transitions
    MatrixXd*        noiseMatrix_ptr  ///< Optional pointer to use custom noise matrix
)
{
    int numMeas = kfEntryList.size();

    if (measTime == GTime::noTime())
    {
        measTime = time;
    }

    time = measTime;

    V.resize(numMeas);
    VV.resize(numMeas);
    Y.resize(numMeas);

    // merge all individual noise elements into the a new map and then vector
    {
        map<KFKey, double> noiseElementMap;

        for (auto& entry : kfEntryList)
            for (auto& [kfKey, value] : entry.noiseElementMap)
            {
                noiseElementMap[kfKey] = value;
            }

        uncorrelatedNoise = VectorXd::Zero(noiseElementMap.size());

        int noises = 0;
        for (auto& [key, value] : noiseElementMap)
        {
            uncorrelatedNoise(noises) = value;

            noiseIndexMap[key] = noises;
            noises++;
        }
    }

    R      = MatrixXd::Zero(numMeas, numMeas);
    H      = MatrixXd::Zero(numMeas, kfState.x.rows());
    H_star = MatrixXd::Zero(numMeas, uncorrelatedNoise.rows());

    prefitRatios  = VectorXd::Zero(numMeas);
    postfitRatios = VectorXd::Zero(numMeas);

    obsKeys.resize(numMeas);
    metaDataMaps.resize(numMeas);
    componentsMaps.resize(numMeas);

    bool error = false;
#ifdef ENABLE_PARALLELISATION
    Eigen::setNbThreads(1);
#pragma omp parallel for
#endif
    for (int meas = 0; meas < kfEntryList.size(); meas++)
    {
        auto it = kfEntryList.begin();
        std::advance(it, meas);

        auto& entry = *it;

        R(meas, meas) = entry.noise;

        auto& value = Y(meas);
        auto& innov = V(meas);

        value = entry.value;
        innov = entry.innov;

        for (auto& [kfKey, coeff] : entry.designEntryMap)
        {
            if (coeff == 0)
            {
                continue;
            }

            int index = kfState.getKFIndex(kfKey);
            if (index < 0)
            {
                std::cout << "Code error: Trying to create measurement for undefined key, check "
                             "stateTransition() is "
                             "called first: "
                          << kfKey << "\n";
                error = true;
            }
            H(meas, index) = coeff;

            if (kfState.assume_linearity)
            {
                double xVal = kfState.x[index];
                double uVal = entry.usedValueMap[kfKey];

                double deltaX = xVal - uVal;
                if (deltaX)
                {
                    BOOST_LOG_TRIVIAL(debug)
                        << std::fixed << "Adjusting meas '" << entry.obsKey << "' as '" << kfKey
                        << "' changed " << deltaX << "\tfrom " << uVal << "\tto " << xVal
                        << "\t : " << innov << "\t-> " << innov - deltaX * coeff;

                    value -= deltaX * coeff;
                    innov -= deltaX * coeff;
                }
            }
        }

        for (auto& [kfKey, coeff] : entry.noiseEntryMap)
        {
            int index = getNoiseIndex(kfKey);
            if (index < 0)
            {
                std::cout << "Code error: Trying to create measurement for undefined key :" << kfKey
                          << "\n";
                error = true;
            }
            H_star(meas, index) = coeff;
        }

        obsKeys[meas]        = std::move(entry.obsKey);
        metaDataMaps[meas]   = std::move(entry.metaDataMap);
        componentsMaps[meas] = std::move(entry.componentsMap);
    }
    Eigen::setNbThreads(0);

    if (error)
    {
        return;
    }

    if (noiseMatrix_ptr)
    {
        R = *noiseMatrix_ptr;
    }

    if (uncorrelatedNoise.rows() != 0)
    {
        SparseMatrix<double> R_A = SparseMatrix<double>(numMeas, uncorrelatedNoise.rows());

        int meas = 0;
        for (auto& entry : kfEntryList)
        {
            for (auto& [kfKey, value] : entry.noiseEntryMap)
            {
                int noiseIndex = noiseIndexMap[kfKey];

                R_A.insert(meas, noiseIndex) = value;
            }

            meas++;
        }

        R = R_A * uncorrelatedNoise.asDiagonal() * R_A.transpose();
    }
}

bool KFState::doStateRejectCallbacks(RejectCallbackDetails rejectDetails)
{
    auto& trace = rejectDetails.trace;

    for (auto& callback : stateRejectCallbacks)
    {
        bool keepGoing = callback(rejectDetails);

        if (keepGoing == false)
        {
            trace << "\n";

            return false;
        }
    }

    trace << "\n";

    return true;
}

bool KFState::doMeasRejectCallbacks(RejectCallbackDetails rejectDetails)
{
    auto& trace = rejectDetails.trace;

    for (auto& callback : measRejectCallbacks)
    {
        bool keepGoing = callback(rejectDetails);

        if (keepGoing == false)
        {
            trace << "\n";

            return false;
        }
    }

    trace << "\n";

    return true;
}

/** Kalman filter operation
 */
void KFState::filterKalman(
    Trace&        trace,       ///< Trace file for output
    KFMeas&       kfMeas,      ///< Measurement object
    const string& suffix,      ///< Suffix to append to residuals block
    bool          innovReady,  ///< Innovation already constructed
    map<string, FilterChunk>*
        filterChunkMap_ptr     ///< Optional map of chunks for parallel processing of sub filters
)
{
    DOCS_REFERENCE(Kalman_Filter__);

    if (kfMeas.time != GTime::noTime())
    {
        time = kfMeas.time;
    }

    map<string, FilterChunk> dummyFilterChunkMap;
    if (filterChunkMap_ptr == nullptr)
    {
        filterChunkMap_ptr = &dummyFilterChunkMap;
    }

    filterChunkMap = *filterChunkMap_ptr;

    if (filterChunkMap.empty())
    {
        FilterChunk filterChunk;

        filterChunk.numX = x.rows();

        filterChunkMap[""] = filterChunk;
    }

    auto returnEarlyPrep = [&]()
    {
        if (rts_basename.empty() == false)
        {
            spitFilterToFile(
                *this,
                E_SerialObject::FILTER_MINUS,
                rts_basename + FORWARD_SUFFIX,
                acsConfig.pppOpts.queue_rts_outputs
            );
            spitFilterToFile(
                *this,
                E_SerialObject::FILTER_PLUS,
                rts_basename + FORWARD_SUFFIX,
                acsConfig.pppOpts.queue_rts_outputs
            );
            spitFilterToFile(
                kfMeas,
                E_SerialObject::MEASUREMENT,
                rts_basename + FORWARD_SUFFIX,
                acsConfig.pppOpts.queue_rts_outputs
            );
        }
    };

    if (kfMeas.H.rows() == 0)
    {
        // nothing to be done, clean up and return early
        returnEarlyPrep();
        return;
    }

    /* kalman filter measurement update */
    if (innovReady == false)
    {
        kfMeas.V  = kfMeas.Y - kfMeas.H * x;
        kfMeas.VV = kfMeas.V;
    }

    TestStatistics testStatistics;

    prefitRatios = VectorXd::Zero(x.rows());

    for (auto& [id, fc] : filterChunkMap)
    {
        if (fc.numH == 0)
        {
            continue;
        }

        if (fc.numX < 0)
            fc.numX = x.rows();
        if (fc.numH < 0)
            fc.numH = kfMeas.H.rows();

        KFStatistics statistics;
        for (int i = 0; i < prefitOpts.max_iterations; i++)
        {
            if (prefitOpts.sigma_check == false && prefitOpts.omega_test == false)
            {
                continue;
            }

            std::stringstream stringBuffer;

            RejectCallbackDetails rejectCallbackDetails(stringBuffer, *this, kfMeas);
            rejectCallbackDetails.stage = E_FilterStage::PREFIT;

            preFitSigmaChecks(
                rejectCallbackDetails,
                statistics,
                fc.begX,
                fc.numX,
                fc.begH,
                fc.numH
            );

            bool stopIterating = true;
            if (rejectCallbackDetails.kfKey.type != int_to_enum<KF>(0))
            {
                stringBuffer << "\n"
                             << "Prefit check failed state test" << "\n";
                doStateRejectCallbacks(rejectCallbackDetails);
                stopIterating = false;
            }
            else if (rejectCallbackDetails.measIndex >= 0)
            {
                stringBuffer << "\n"
                             << "Prefit check failed measurement test" << "\n";
                doMeasRejectCallbacks(rejectCallbackDetails);
                stopIterating = false;
            }

            if (stopIterating)
            {
                stringBuffer << "\n"
                             << "Prefit check passed" << "\n";
            }
            else
            {
                if (i == prefitOpts.max_iterations - 1)
                {
                    BOOST_LOG_TRIVIAL(warning)
                        << "Max pre-fit filter iterations limit reached at " << time << " in "
                        << suffix << ", limit is " << prefitOpts.max_iterations;
                    stringBuffer << "\n"
                                 << "Warning: Max pre-fit filter iterations limit reached at "
                                 << time << " in " << suffix << ", limit is "
                                 << prefitOpts.max_iterations << "\n";

                    stopIterating = true;
                }
            }

            trace << stringBuffer.str();
            if (fc.trace_ptr)
                *fc.trace_ptr << stringBuffer.str();

            if (stopIterating)
            {
                break;
            }
        }

        testStatistics.sumOfSquaresPre += statistics.sumOfSquares;
        testStatistics.averageRatioPre += statistics.averageRatio / filterChunkMap.size();
    }

    if (prefitOpts.sigma_check || prefitOpts.omega_test)
    {
        trace << "\n"
              << "Sum-of-squared test statistics (prefit): " << testStatistics.sumOfSquaresPre
              << "\n";
    }

    VectorXd xp = x;
    MatrixXd Pp = P;
    dx          = VectorXd::Zero(x.rows());

    postfitRatios = VectorXd::Zero(x.rows());

    statisticsMap["States"] = x.rows();
    BOOST_LOG_TRIVIAL(info) << " ------- FILTERING BY CHUNK " << filterChunkMap.size()
                            << "         --------\n";
    for (auto& [id, fc] : filterChunkMap)
    {
        if (fc.numH == 0)
        {
            continue;
        }

        if (fc.id.empty() == false)
        {
            BOOST_LOG_TRIVIAL(info)
                << " ------- FILTERING CHUNK " << fc.id << "         --------\n";
        }

        MatrixXd Qinv  = MatrixXd::Identity(fc.numH, fc.numH);
        MatrixXd QinvH = MatrixXd::Ones(fc.numH, fc.numX);

        statisticsMap["Observations"] += fc.numH;

        KFStatistics statistics;
        for (int i = 0; i < postfitOpts.max_iterations; i++)
        {
            bool pass =
                kFilter(trace, kfMeas, xp, Pp, dx, Qinv, QinvH, fc.begX, fc.numX, fc.begH, fc.numH);

            if (pass == false)
            {
                trace << "FILTER FAILED" << "\n";
                returnEarlyPrep();
                return;
            }

            if (advanced_postfits == false)
            {
                kfMeas.VV.segment(fc.begH, fc.numH) =
                    kfMeas.V.segment(fc.begH, fc.numH) -
                    kfMeas.H.block(fc.begH, fc.begX, fc.numH, fc.numX) *
                        dx.segment(fc.begX, fc.numX);
            }

            bool stopIterating = true;

            if (postfitOpts.sigma_check || postfitOpts.omega_test)
            {
                std::stringstream stringBuffer;

                RejectCallbackDetails rejectCallbackDetails(stringBuffer, *this, kfMeas);
                rejectCallbackDetails.stage = E_FilterStage::POSTFIT;

                postFitSigmaChecks(
                    rejectCallbackDetails,
                    dx,
                    Qinv,
                    QinvH,
                    statistics,
                    fc.begX,
                    fc.numX,
                    fc.begH,
                    fc.numH
                );

                if (rejectCallbackDetails.kfKey.type != int_to_enum<KF>(0))
                {
                    stringBuffer << "\n"
                                 << "Postfit check failed state test" << "\n";
                    doStateRejectCallbacks(rejectCallbackDetails);
                    stopIterating = false;
                }
                else if (rejectCallbackDetails.measIndex >= 0)
                {
                    stringBuffer << "\n"
                                 << "Postfit check failed measurement test" << "\n";
                    doMeasRejectCallbacks(rejectCallbackDetails);
                    stopIterating = false;
                }

                if (stopIterating)
                {
                    stringBuffer << "\n"
                                 << "Postfit check passed" << "\n";
                }
                else
                {
                    if (i == postfitOpts.max_iterations - 1)
                    {
                        BOOST_LOG_TRIVIAL(warning)
                            << "Max post-fit filter iterations limit reached at " << time << " in "
                            << suffix << ", limit is " << postfitOpts.max_iterations;
                        stringBuffer << "\n"
                                     << "Warning: Max post-fit filter iterations limit reached at "
                                     << time << " in " << suffix << ", limit is "
                                     << postfitOpts.max_iterations << "\n";

                        stopIterating = true;
                    }
                }

                trace << stringBuffer.str();
                if (fc.trace_ptr)
                    *fc.trace_ptr << stringBuffer.str();
            }

            if (output_residuals)
            {
                outputResiduals(trace, kfMeas, suffix, i, fc.begH, fc.numH);
            }

            if (traceLevel >= 5)
            {
                KFState kfStateCopy = *this;
                kfStateCopy.x       = xp;
                kfStateCopy.P       = Pp;

                kfStateCopy.outputStates(trace, suffix, i, fc.begH, fc.numH);
            }

            if (stopIterating)
            {
                statisticsMap["Filter iterations " + std::to_string(i + 1)]++;

                break;
            }
        }

        if (outputMongoMeasurements)
        {
            mongoMeasResiduals(
                kfMeas.time,
                kfMeas,
                acsConfig.mongoOpts.queue_outputs,
                suffix,
                fc.begH,
                fc.numH
            );
        }

        testStatistics.sumOfSquaresPost += statistics.sumOfSquares;
        testStatistics.averageRatioPost += statistics.averageRatio / filterChunkMap.size();
    }

    if (postfitOpts.sigma_check || postfitOpts.omega_test)
        trace << "\n"
              << "Sum-of-squared test statistics (postfit): " << testStatistics.sumOfSquaresPost
              << "\n";

    if (chiSquareTest.enable)
    {
        for (auto& [id, fc] : filterChunkMap)
        {
            if (fc.numH == 0)
            {
                continue;
            }

            auto& chunkTrace = *fc.trace_ptr;

            switch (chiSquareTest.mode
            )  // todo Eugene: rethink Chi-Square test modes, consider keep only INNOVATION
               // and determine DOF automatically based on process noises
            {
                case E_ChiSqMode::INNOVATION:
                {
                    testStatistics.chiSq +=
                        innovChiSquare(chunkTrace, kfMeas, fc.begX, fc.numX, fc.begH, fc.numH);
                    break;
                }
                case E_ChiSqMode::MEASUREMENT:
                {
                    testStatistics.chiSq +=
                        measChiSquare(chunkTrace, kfMeas, dx, fc.begX, fc.numX, fc.begH, fc.numH);
                    break;
                }
                case E_ChiSqMode::STATE:
                {
                    testStatistics.chiSq +=
                        stateChiSquare(chunkTrace, Pp, dx, fc.begX, fc.numX, fc.begH, fc.numH);
                    break;
                }
                default:
                {
                    BOOST_LOG_TRIVIAL(error) << "Unknown Chi-square test mode";
                    break;
                }
            }
        }

        if (chiSquareTest.mode == E_ChiSqMode::STATE)
            testStatistics.dof = x.rows() - 1;
        else
            testStatistics.dof =
                kfMeas.H.rows();  // todo Eugene: revisit DOF in the future for MEASUREMENT mode

        testStatistics.chiSqPerDof = testStatistics.chiSq / testStatistics.dof;

        // check against threshold
        boost::math::normal normDist;
        double alpha = cdf(complement(normDist, chiSquareTest.sigma_threshold)) * 2;  // two-tailed

        boost::math::chi_squared chiSqDist(testStatistics.dof);
        testStatistics.qc = quantile(complement(chiSqDist, alpha));
        if (testStatistics.dof > 0 && testStatistics.chiSq <= testStatistics.qc)
            trace << "\n"
                  << "Chi-square test passed";
        else
            trace << "\n"
                  << "Chi-square test failed";

        trace << "\n"
              << "Chi-square increment: " << testStatistics.chiSq
              << "\tThreshold: " << testStatistics.qc
              << "\tNumber of measurements:" << kfMeas.H.rows()
              << "\tNumber of states:" << x.rows() - 1
              << "\tDegree of freedom: " << testStatistics.dof
              << "\tChi-square per DOF: " << testStatistics.chiSqPerDof << "\n";
    }

    if (acsConfig.mongoOpts.output_test_stats != E_Mongo::NONE)
    {
        mongoTestStat(*this, testStatistics);
    }

    if (rts_basename.empty() == false)
    {
        spitFilterToFile(
            *this,
            E_SerialObject::FILTER_MINUS,
            rts_basename + FORWARD_SUFFIX,
            acsConfig.pppOpts.queue_rts_outputs
        );
    }

    if (simulate_filter_only)
    {
        dx = VectorXd::Zero(x.rows());
    }
    else
    {
        x = std::move(xp);
        P = std::move(Pp);
    }

    if (rts_basename.empty() == false)
    {
        spitFilterToFile(
            *this,
            E_SerialObject::FILTER_PLUS,
            rts_basename + FORWARD_SUFFIX,
            acsConfig.pppOpts.queue_rts_outputs
        );
        spitFilterToFile(
            kfMeas,
            E_SerialObject::MEASUREMENT,
            rts_basename + FORWARD_SUFFIX,
            acsConfig.pppOpts.queue_rts_outputs
        );
    }

    initFilterEpoch(trace);
}

/** Least squares estimator for new kalman filter states.
 * If new states have been added that do not contain variance values, the filter will assume that
 * these states values and covariances should be estimated using least squares.
 *
 * This function will extract the minimum required states from the existing state vector,
 * and the minimum required measurements in order to perform least squares for the uninitialised
 * states.
 */
bool KFState::leastSquareInitStates(
    Trace&        trace,        ///< Trace file for output
    KFMeas&       kfMeas,       ///< Measurement object
    const string& suffix,       ///< Suffix to append to residuals block
    bool          initCovars,   ///< Option to also initialise off-diagonal covariance values
    bool          innovReady,   ///< Apriori states available and residuals already calculated
    bool          skipLsqCheck  ///< Skip outlier screening in case not converged or within SPP RAIM
)
{
    lsqRequired = false;

    sigmaPass = false;  // Eugene: can also do this for filterKalman
    chiQCPass = false;  // Eugene: can also do this for filterKalman

    if (innovReady == false)
    {
        kfMeas.V = kfMeas.Y;  // x == 0 in this case
    }

    // find all the states that aren't initialised, they need least squaring.
    vector<int> newStateIndicies;
    for (auto& [key, index] : kfIndexMap)
    {
        if ((key.type != KF::ONE) && (P(index, index) < 0))
        {
            // this is a new state and needs to be estimated using least squares
            newStateIndicies.push_back(index);
        }
    }

    // find the subset of measurements that are required for the initialisation
    auto usedMeasMask = kfMeas.H(all, newStateIndicies).rowwise().any();

    vector<int> leastSquareMeasIndicies;
    vector<int> pseudoMeasStateIndicies;

    for (int measIndex = 0; measIndex < usedMeasMask.rows(); measIndex++)
    {
        // if not used, dont worry about it
        if (usedMeasMask(measIndex) == 0)
        {
            continue;
        }

        // this measurement is used to calculate a new state.
        // copy it to a new design matrix
        leastSquareMeasIndicies.push_back(measIndex);

        // remember make a pseudo measurement of anything it references that is already set
        for (int stateIndex = 0; stateIndex < kfMeas.H.cols(); stateIndex++)
        {
            if ((kfMeas.H(measIndex, stateIndex) != 0) && (P(stateIndex, stateIndex) >= 0))
            {
                pseudoMeasStateIndicies.push_back(stateIndex);
            }
        }
    }

    int lsqMeasCount    = leastSquareMeasIndicies.size();
    int pseudoMeasCount = pseudoMeasStateIndicies.size();
    int totalMeasCount  = lsqMeasCount + pseudoMeasCount;

    // Create new measurement objects with larger size, (using all states for now)
    KFMeas leastSquareMeas;

    leastSquareMeas.V = VectorXd::Zero(totalMeasCount);
    leastSquareMeas.R = MatrixXd::Zero(totalMeasCount, totalMeasCount);
    leastSquareMeas.H = MatrixXd::Zero(totalMeasCount, kfMeas.H.cols());

    // copy in the required measurements from the old set
    leastSquareMeas.V.head(lsqMeasCount) = kfMeas.V(leastSquareMeasIndicies);
    leastSquareMeas.R.topLeftCorner(lsqMeasCount, lsqMeasCount) =
        kfMeas.R(leastSquareMeasIndicies, leastSquareMeasIndicies);
    leastSquareMeas.H.topRows(lsqMeasCount) = kfMeas.H(leastSquareMeasIndicies, all);

    // append any new pseudo measurements to the end
    for (int i = 0; i < pseudoMeasCount; i++)
    {
        int measIndex  = lsqMeasCount + i;
        int stateIndex = pseudoMeasStateIndicies[i];

        if (innovReady)
        {
            leastSquareMeas.V(measIndex) = 0;  // take x as apriori state
        }
        else
        {
            leastSquareMeas.V(measIndex) = x(stateIndex);  // take 0 as apriori state
        }
        leastSquareMeas.R(measIndex, measIndex) =
            P(stateIndex, stateIndex);  // todo Eugene: check equivalence w/ back-subsitution -
                                        // pseudo var should be 0 instead of P, or doesn't matter?
        leastSquareMeas.H(measIndex, stateIndex) = 1;
    }

    // find the subset of states required for these measurements
    vector<int> usedStateIndicies;
    auto        usedStateMask = leastSquareMeas.H.colwise().any();
    for (int i = 0; i < usedStateMask.cols(); i++)
    {
        if (usedStateMask(i) != 0)
        {
            usedStateIndicies.push_back(i);
        }
    }

    // create a new meaurement object using only the required states.
    KFMeas leastSquareMeasSubs;
    leastSquareMeasSubs.time = kfMeas.time;
    leastSquareMeasSubs.V    = leastSquareMeas.V;
    leastSquareMeasSubs.R    = leastSquareMeas.R;
    leastSquareMeasSubs.H    = leastSquareMeas.H(all, usedStateIndicies);

    for (int i = 0; i < lsqMeasCount; i++)
    {
        int measIndex = leastSquareMeasIndicies[i];
        leastSquareMeasSubs.obsKeys.push_back(kfMeas.obsKeys[measIndex]);
        leastSquareMeasSubs.metaDataMaps.push_back(kfMeas.metaDataMaps[measIndex]);
        leastSquareMeasSubs.componentsMaps.push_back(kfMeas.componentsMaps[measIndex]);
    }

    int      usedStateCount = usedStateIndicies.size();
    VectorXd xp             = VectorXd::Zero(usedStateCount);
    MatrixXd Pp             = MatrixXd::Identity(usedStateCount, usedStateCount);

    TestStatistics testStatistics;
    KFStatistics   statistics;
    for (int i = 0; i < lsqOpts.max_iterations; i++)
    {
        bool pass = leastSquare(trace, leastSquareMeasSubs, xp, Pp);

        if (pass == false)
        {
            trace << "LSQ FAILED" << "\n";
            return false;
        }

        leastSquareMeasSubs.VV = leastSquareMeasSubs.V - leastSquareMeasSubs.H * xp;

        if (chiSquareTest.enable)
        {
            chiQC(trace, leastSquareMeasSubs);

            if (chiQCPass)
                trace << "\nChi-square test passed: ";
            else
                trace << "\nChi-square test failed: ";

            trace << "dof=" << dof << "\tchi^2=" << chi2 << "\tthres=" << qc
                  << "\tsigma0=" << sqrt(chi2PerDof);
        }

        bool stopIterating = true;

        if ((lsqOpts.sigma_check || lsqOpts.omega_test) && skipLsqCheck == false)
        {
            std::stringstream stringBuffer;

            RejectCallbackDetails rejectCallbackDetails(stringBuffer, *this, leastSquareMeasSubs);
            rejectCallbackDetails.stage = E_FilterStage::LSQ;

            leastSquareSigmaChecks(rejectCallbackDetails, Pp, statistics);

            if (rejectCallbackDetails.measIndex >= 0)
            {
                stringBuffer << "\n"
                             << "Least squares check failed";
                doMeasRejectCallbacks(rejectCallbackDetails);
                stopIterating = false;
            }

            if (stopIterating)
            {
                stringBuffer << "\n"
                             << "Least squares check passed";
                sigmaPass = true;
            }
            else
            {
                if (i == lsqOpts.max_iterations - 1)
                {
                    BOOST_LOG_TRIVIAL(debug)
                        << "Max least squares iterations limit reached at " << time << " in "
                        << suffix << ", limit is " << lsqOpts.max_iterations;
                    stringBuffer << "\n"
                                 << "Warning: Max least squares iterations limit reached at "
                                 << time << " in " << suffix << ", limit is "
                                 << lsqOpts.max_iterations;

                    stopIterating = true;
                    sigmaPass     = false;
                }
            }

            trace << stringBuffer.str();
        }

        if (output_residuals && traceLevel >= 5)
        {
            outputResiduals(trace, leastSquareMeasSubs, suffix, i, 0, leastSquareMeasSubs.H.rows());
        }

        if (stopIterating)
        {
            // statisticsMap["Least squares iterations " + std::to_string(i+1)]++;

            break;
        }
    }

    testStatistics.sumOfSquaresLsq = statistics.sumOfSquares;
    testStatistics.averageRatioLsq = statistics.averageRatio;

    if ((lsqOpts.sigma_check || lsqOpts.omega_test) && skipLsqCheck == false)
        trace << "\n"
              << "Sum-of-squared test statistics (least squares): "
              << testStatistics.sumOfSquaresLsq << "\n";

    for (int i = 0; i < usedStateCount; i++)
    {
        int stateRowIndex = usedStateIndicies[i];

        if (P(stateRowIndex, stateRowIndex) >= 0)
        {
            continue;
        }

        double newStateVal = xp(i);
        double newStateCov = Pp(i, i);

        dx(stateRowIndex) = newStateVal;

        if (innovReady)
        {
            x(stateRowIndex) += newStateVal;
        }
        else
        {
            x(stateRowIndex) = newStateVal;
        }

        P(stateRowIndex, stateRowIndex) = newStateCov;

        if (initCovars)
        {
            for (int j = 0; j < i; j++)
            {
                int stateColIndex = usedStateIndicies[j];

                newStateCov = Pp(i, j);

                P(stateRowIndex, stateColIndex) = newStateCov;
                P(stateColIndex, stateRowIndex) = newStateCov;
            }
        }
    }

    kfMeas.VV = kfMeas.V - kfMeas.H * dx;
    kfMeas.R(leastSquareMeasIndicies, leastSquareMeasIndicies) =
        leastSquareMeasSubs.R.topLeftCorner(lsqMeasCount, lsqMeasCount);
    if (leastSquareMeasSubs.postfitRatios.rows() >= lsqMeasCount)
    {
        kfMeas.postfitRatios(leastSquareMeasIndicies) =
            leastSquareMeasSubs.postfitRatios.head(lsqMeasCount);
    }

    return true;
}

/** Get a portion of the state vector by passing a list of keys
 */
VectorXd KFState::getSubState(
    map<KFKey, int>& kfKeyMap,  ///< List of keys to return within substate
    MatrixXd* covarMat_ptr,     ///< Optional pointer to a matrix for output of covariance submatrix
    VectorXd* adjustVec_ptr     ///< Optional pointer to a vector for output of last adjustments
) const
{
    vector<int> indices;
    indices.resize(kfKeyMap.size());

    for (auto& [kfKey, mapIndex] : kfKeyMap)
    {
        int stateIndex = getKFIndex(kfKey);
        if (stateIndex >= 0)
        {
            indices[mapIndex] = stateIndex;
        }
    }

    VectorXd subState = x(indices);
    if (covarMat_ptr)
    {
        *covarMat_ptr = P(indices, indices);
    }
    if (adjustVec_ptr)
    {
        *adjustVec_ptr = dx(indices);
    }

    return subState;
}

/** Get a portion of a state by passing in a list of keys.
 *  Only gets some aspects, as most aren't required
 */
void KFState::getSubState(
    map<KFKey, int>& kfKeyMap,  ///< List of keys to return within substate
    KFState&         subState   ///< Output state
) const
{
    vector<int> indices;
    indices.resize(kfKeyMap.size());

    subState.kfIndexMap.clear();
    for (auto& [kfKey, mapIndex] : kfKeyMap)
    {
        int stateIndex = getKFIndex(kfKey);
        if (stateIndex >= 0)
        {
            indices[mapIndex] = stateIndex;
        }

        subState.kfIndexMap[kfKey] = mapIndex;
    }

    subState.time = time;
    subState.x    = x(indices);
    subState.dx   = dx(indices);
    subState.P    = P(indices, indices);

    subState.stateTransitionMap.clear();

    for (auto& [keyA, stmMap] : stateTransitionMap)
    {
        auto itA = kfKeyMap.find(keyA);
        if (itA == kfKeyMap.end())
        {
            continue;
        }

        for (auto& [keyB, st] : stmMap)
        {
            auto itB = kfKeyMap.find(keyB);
            if (itB == kfKeyMap.end())
            {
                continue;
            }

            subState.stateTransitionMap[keyA][keyB] = st;
        }
    }
}

KFState KFState::getSubState(vector<KF> types, KFMeas* meas_ptr) const
{
    if (std::find(types.begin(), types.end(), KF::ALL) != types.end())
    {
        return *this;
    }

    KFState subState;

    vector<int> indices;

    int index = 0;
    for (auto& [kfKey, mapIndex] : kfIndexMap)
    {
        if (std::find(types.begin(), types.end(), kfKey.type) == types.end())
        {
            continue;
        }

        indices.push_back(mapIndex);
        subState.kfIndexMap[kfKey] = index;

        index++;
    }

    subState.time = time;
    subState.x    = x(indices);
    subState.dx   = dx(indices);
    subState.P    = P(indices, indices);

    if (meas_ptr)
    {
        auto& meas = *meas_ptr;

        meas.H = meas.H(all, indices);
    }

    return subState;
}

/** Output keys and states in human readable format
 */
void KFState::outputStates(
    Trace& trace,      ///< Trace to output to
    string suffix,     ///< Suffix to append to state block info tag in trace files
    int    iteration,  ///< Number of iterations prior to this check
    int    begX,       ///< Index of first state element to process
    int    numX        ///< Number of state elements to process
)
{
    tracepdeex(1, trace, "\n");

    string name = "STATES";
    name += suffix;

    Block block(trace, name);

    tracepdeex(
        1,
        trace,
        "#\t%2s\t%22s\t%12s\t%4s\t%4s\t%7s\t%17s\t%17s\t%16s",
        "It",
        "Time",
        "Type",
        "Sat",
        "Str",
        "Code",
        "State",
        "Sigma",
        "Adjust"
    );
    tracepdeex(5, trace, "\t%16s", "Prefit Ratio");
    tracepdeex(5, trace, "\t%16s", "Postfit Ratio");
    tracepdeex(5, trace, "\t%17s", "Mu");
    tracepdeex(2, trace, "\t%s", "Comments");
    tracepdeex(1, trace, "\n");

    int endX;
    if (numX < 0)
        endX = x.rows();
    else
        endX = begX + numX;

    bool noAdjust = dx.isZero();

    for (auto& [key, index] : kfIndexMap)
    {
        if (index >= x.rows())
        {
            continue;
        }
        if (index < begX || index >= endX)
        {
            continue;
        }

        char xStr[20];
        char sigmaStr[20];
        char dxStr[20];
        char preRatioStr[20];
        char postRatioStr[20];
        char muStr[20];

        double _x = x(index);

        if (_x == 0 || (fabs(_x) > 0.001 && fabs(_x) < 1e8))
            snprintf(xStr, sizeof(xStr), "%17.7f", _x);
        else
            snprintf(xStr, sizeof(xStr), "%17.3e", _x);

        double _sigma = sqrt(P(index, index));

        if (_sigma == 0 || (fabs(_sigma) > 0.0001 && fabs(_sigma) < 1e8))
            snprintf(sigmaStr, sizeof(sigmaStr), "%17.8f", _sigma);
        else
            snprintf(sigmaStr, sizeof(sigmaStr), "%17.4e", _sigma);

        double _dx = 0;
        if (index < dx.rows())
            _dx = dx(index);

        if (noAdjust)
            snprintf(dxStr, sizeof(dxStr), "%16.0s", "");
        else if (_dx == 0 || (fabs(_dx) > 0.0001 && fabs(_dx) < 1e6))
            snprintf(dxStr, sizeof(dxStr), "%16.8f", _dx);
        else
            snprintf(dxStr, sizeof(dxStr), "%16.4e", _dx);

        double preRatio = 0;
        if (index < prefitRatios.rows())
            preRatio = prefitRatios(index);

        if (preRatio == 0 || (fabs(preRatio) > 0.001 && fabs(preRatio) < 1e7))
            snprintf(preRatioStr, sizeof(preRatioStr), "%16.7f", preRatio);
        else
            snprintf(preRatioStr, sizeof(preRatioStr), "%16.3e", preRatio);

        double postRatio = 0;
        if (index < postfitRatios.rows())
            postRatio = postfitRatios(index);

        if (postRatio == 0 || (fabs(postRatio) > 0.001 && fabs(postRatio) < 1e7))
            snprintf(postRatioStr, sizeof(postRatioStr), "%16.7f", postRatio);
        else
            snprintf(postRatioStr, sizeof(postRatioStr), "%16.3e", postRatio);

        double mu = 0;
        auto   it = gaussMarkovMuMap.find(key);
        if (it != gaussMarkovMuMap.end())
            mu = it->second;

        if (mu == 0)
            snprintf(muStr, sizeof(muStr), "");
        else if (fabs(mu) > 0.001 && fabs(mu) < 1e8)
            snprintf(muStr, sizeof(muStr), "%17.7f", mu);
        else
            snprintf(muStr, sizeof(muStr), "%17.3e", mu);

        tracepdeex(
            1,
            trace,
            "*\t%2d\t%22s\t%30s\t%17s\t%17s\t%16s",
            iteration,
            time.to_string(2).c_str(),
            ((string)key).c_str(),
            xStr,
            sigmaStr,
            dxStr
        );
        tracepdeex(5, trace, "\t%16s", preRatioStr);
        tracepdeex(5, trace, "\t%16s", postRatioStr);
        tracepdeex(5, trace, "\t%17s", muStr);
        tracepdeex(6, trace, "\t%x", key.rec_ptr);
        tracepdeex(2, trace, "\t%s", key.comment.c_str());
        tracepdeex(1, trace, "\n");
    }
}

MatrixXi correlationMatrix(MatrixXd& P)
{
    MatrixXi correlations = MatrixXi(P.rows(), P.cols());

    for (int i = 0; i < P.rows(); i++)
        for (int j = 0; j <= i; j++)
        {
            double v1  = P(i, i);
            double v2  = P(j, j);
            double v12 = P(i, j);

            double correlation = v12 / sqrt(v1 * v2) * 100;
            correlations(i, j) = correlation;
            correlations(j, i) = correlation;
        }

    return correlations;
}

void KFState::outputConditionNumber(Trace& trace)
{
    Eigen::JacobiSVD<MatrixXd> svd(P.bottomRightCorner(P.rows() - 1, P.cols() - 1));
    double                     conditionNumber =
        svd.singularValues()(0) / svd.singularValues()(svd.singularValues().size() - 1);

    tracepdeex(0, trace, "\n\n Condition number: %f", conditionNumber);
}

void KFState::outputCorrelations(Trace& trace)
{
    tracepdeex(2, trace, "\n");

    Block block(trace, "CORRELATIONS");

    int skip  = 0;
    int total = kfIndexMap.size();
    for (auto& [key, index] : kfIndexMap)
    {
        if (key.type == KF::ONE)
        {
            continue;
        }

        tracepdeex(2, trace, "%s      ", KFKey::emptyString().c_str());
        for (int i = 0; i < skip; i++)
        {
            tracepdeex(2, trace, "|    ");
        }

        trace << "> ";

        for (int i = 0; i < total - skip; i++)
        {
            tracepdeex(2, trace, "-----");
        }

        trace << key << "\n";

        skip++;
    }

    MatrixXi correlations = correlationMatrix(P);

    for (auto& [key, index] : kfIndexMap)
    {
        if (key.type == KF::ONE)
        {
            continue;
        }

        trace << key << " : ";

        for (auto& [key2, index2] : kfIndexMap)
        {
            if (key2.type == KF::ONE)
            {
                continue;
            }

            int correlation = correlations(index, index2);

            if (index == index2)
                tracepdeex(2, trace, "%4s ", "100");
            else if (fabs(correlation) > 100)
                tracepdeex(2, trace, "%4s ", "----");
            else if (fabs(correlation) < 1)
                tracepdeex(2, trace, "%4s ", "");
            else
                tracepdeex(2, trace, "%4.0f ", correlation);
        }

        trace << "\n";
    }
}

void KFState::outputMeasurements(Trace& trace, KFMeas& meas)
{
    tracepdeex(2, trace, "\n");

    Block block(trace, "MEASUREMENTS");

    int total = kfIndexMap.size();
    int skip  = 0;
    for (auto& [key, index] : kfIndexMap)
    {
        if (key.type == KF::ONE)
        {
            continue;
        }

        tracepdeex(0, trace, "%s        ", KFKey::emptyString().c_str());

        for (int i = 0; i < skip; i++)
        {
            tracepdeex(2, trace, "|      ");
        }

        trace << "> ";

        for (int i = 0; i < total - skip; i++)
        {
            tracepdeex(2, trace, "-------");
        }

        trace << key << "\n";

        skip++;
    }

    for (int i = 0; i < meas.obsKeys.size(); i++)
    {
        auto& key = meas.obsKeys[i];

        trace << key << " : ";

        for (int j = 1; j < meas.H.cols(); j++)
        {
            double a = meas.H(i, j);

            if (fabs(a) > 0.001)
                tracepdeex(2, trace, "%6.2f ", a);
            else
                tracepdeex(2, trace, "%6.2s ", "");
        }
        tracepdeex(2, trace, "\t   : %16.4f\n", meas.V(i));
    }
}

InitialState initialStateFromConfig(const KalmanModel& kalmanModel, int index)
{
    InitialState init = {};

    if (index < kalmanModel.estimate.size())
        init.estimate = kalmanModel.estimate[index];
    else
        init.estimate = kalmanModel.estimate.back();
    if (index < kalmanModel.use_remote_sigma.size())
        init.use_remote_sigma = kalmanModel.use_remote_sigma[index];
    else
        init.use_remote_sigma = kalmanModel.use_remote_sigma.back();
    if (index < kalmanModel.apriori_value.size())
        init.x = kalmanModel.apriori_value[index];
    else
        init.x = kalmanModel.apriori_value.back();
    if (index < kalmanModel.sigma.size())
        init.P = SQR(kalmanModel.sigma[index]) * SGN(kalmanModel.sigma[index]);
    else
        init.P = SQR(kalmanModel.sigma.back()) * SGN(kalmanModel.sigma.back());
    if (index < kalmanModel.sigma_limit.size())
        init.sigmaMax = kalmanModel.sigma_limit[index];
    else
        init.sigmaMax = kalmanModel.sigma_limit.back();
    if (index < kalmanModel.outage_limit.size())
        init.outageLimit = kalmanModel.outage_limit[index];
    else
        init.outageLimit = kalmanModel.outage_limit.back();
    if (index < kalmanModel.tau.size())
        init.tau = kalmanModel.tau[index];
    else
        init.tau = kalmanModel.tau.back();
    if (index < kalmanModel.mu.size())
        init.mu = kalmanModel.mu[index];
    else
        init.mu = kalmanModel.mu.back();
    if (index < kalmanModel.process_noise.size())
        init.Q = SQR(kalmanModel.process_noise[index]) * SGN(kalmanModel.process_noise[index]);
    else
        init.Q = SQR(kalmanModel.process_noise.back()) * SGN(kalmanModel.process_noise.back());
    if (index < kalmanModel.comment.size())
        init.comment = kalmanModel.comment[index];
    else
        init.comment = kalmanModel.comment.back();

    return init;
}

KFState mergeFilters(const vector<KFState*>& kfStatePointerList, const vector<KF>& stateList)
{
    map<KFKey, double>             stateValueMap;
    map<KFKey, map<KFKey, double>> stateCovarMap;

    for (auto& statePointer : kfStatePointerList)
    {
        KFState& kfState = *statePointer;

        for (auto& [key1, index1] : kfState.kfIndexMap)
            for (auto state1 : stateList)
            {
                if (key1.type == state1)
                {
                    stateValueMap[key1] = kfState.x(index1);

                    for (auto& [key2, index2] : kfState.kfIndexMap)
                        for (auto state2 : stateList)
                        {
                            if (key2.type == state2)
                            {
                                double val = kfState.P(index1, index2);
                                if (val != 0)
                                {
                                    stateCovarMap[key1][key2] = val;
                                }

                                break;
                            }
                        }
                    break;
                }
            }
    }

    KFState mergedKFState;

    mergedKFState.x  = VectorXd::Zero(stateValueMap.size());
    mergedKFState.dx = VectorXd::Zero(stateValueMap.size());
    mergedKFState.P  = MatrixXd::Zero(stateValueMap.size(), stateValueMap.size());

    int i = 0;
    for (auto& [key, value] : stateValueMap)
    {
        mergedKFState.kfIndexMap[key] = i;
        mergedKFState.x(i)            = value;

        i++;
    }

    for (auto& [key1, map2] : stateCovarMap)
        for (auto& [key2, value] : map2)
        {
            int index1 = mergedKFState.kfIndexMap[key1];
            int index2 = mergedKFState.kfIndexMap[key2];

            mergedKFState.P(index1, index2) = value;
        }

    return mergedKFState;
}

bool isPositiveSemiDefinite(MatrixXd& mat)
{
    for (int i = 0; i < mat.rows(); i++)
        for (int j = 0; j < i; j++)
        {
            double a  = mat(i, i);
            double ab = mat(i, j);
            double b  = mat(j, j);

            if (ab * ab > a * b)
            {
                // 			std::cout << "large off diagonals " << "\n";
                // 			return false;
                if (ab > 0)
                    ab = +sqrt(0.99 * a * b);
                else
                    ab = -sqrt(0.99 * a * b);
                mat(i, j) = ab;
                mat(j, i) = ab;
            }
        }
    return true;
}
