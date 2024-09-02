
// #pragma GCC optimize ("O0")

#include "algebra.hpp"
#include "common.hpp"
#include "trace.hpp"
#include "ppp.hpp"

#include <iostream>
#include <random>


string noteONE = R"raw(
This is the basic form of the outputStates function's output, describing the times, keys, values, variances, and adjustments of everything in the filter.
Note that the filter is not empty upon initialisation, it contains a ONE element that is always present with state value 1, and used for internal calculations.
Get used to ignoring this element.
	)raw";

string noteInit = R"raw(
When the stateTransition function is called, any kfKeys that were defined and used in measurements are automatically added as states.
They contain the values as defined in the InitialState that may have been used.
	)raw";

string noteAddMeas = R"raw(
Here the code creates several KFMeasEntrys and adds them to a list
	)raw";

string noteMeas = R"raw(
When the combineKFMeasList function is called, finalised matrices of the measurements are generated, ready for filtering
They can be viewed with the outputMeasurements function, which displays the measurements keys down the left, states across the top, OMCs on the right, and non zero entries of the design matrix between.
(Its a lot more obvious whats going on when there are more observations and states, and this output is generally unnecessary)
	)raw";

string noteResiduals = R"raw(
When the filterKalman function is called, standard ekf processes are used to update the states and covariances of the filter.
During the process, the residuals and statistics may be output, showing the fit achieved to the measurements.
Here we see a significant reduction in pre->post fit residuals, indicating the adjustments have improved the fit to these measurements.
	)raw";


string noteIntro1 = R"raw(
This walkthrough demonstrates the basics of Ginan's kalman filter.
Its target is an object having its position measured in 1D, by measurements over several epochs.
It's expected that the comments in the code, and the descriptions in the outputs below will give a good basis for starting to work with Ginan's kalman filter.
	)raw";

string notePostFilter1 = R"raw(
Here we see that the "Walk" REC_POS state value after the filter has moved substantially toward the true value (10), and its variance has decreased from its previous value.
	)raw";

string noteProcNoise = R"raw(
In the second epoch, after some time has elapsed, the stateTransition function has added process noise according to the value specified by InitialState.
We see a large increase in the variance.
	)raw";

string noteReject = R"raw(
During this epoch, one of the measurements encounters an outlier, with some large glitch in the observed term.
During processing of the filter the consistency of measurements' and states' variances with each other is confirmed, and outliers detected.
In the case that this consistency check fails, rejection callbacks may be defined, which may trigger appropriate actions.
Here we see that the measurement with the glitch has its measurement variance deweighted by a factor of 10000 such that it doesnt corrupt the filter states.
In the first filter residuals block, both states have poor postfit residuals, but upon iteration when the second measurement is deweighted, the remaining first measurement achieves a good fit.
	)raw";

string noteIntro2 = R"raw(
Beginning of walkthrough section 2

This walkthrough demonstrates the basics of using rate terms in Ginan's kalman filter (velocities, accelerations)

Its target is an object having its position measured in 1D, by measurements over several epochs.
It's expected that the comments in the code, and the descriptions in the outputs below will give a good basis for starting to work with Ginan's kalman filter.
	)raw";

string noteRemove = R"raw(
The states from section 1 remain in the filter.
At this point the code removes all states, but this wont be seen until the next stateTransition call
	)raw";

string noteMeas2 = R"raw(
Here we see the design matrix created by the code - there are 6 states that have been automatically added to the filter by 3 measurements.
1 - This measurement measures the state with num = 1 directly. There is no rate associated with this state
2 - This measurement measures the position state with num = 2 directly. There is a velocity term associated with this rate, but it cant be measured directly with a position observation.
3 - This measurement measures the position state with num = 3 directly. There are velocity and acceleration terms associated with this rate, but they also cant be measured directly.

Thus the only entries in the design matrix are those corresponding to the position states.
	)raw";

string noteFirstMeas = R"raw(
Here we see that after one measurement the variance of the position state has reduced substantially, and the position state value quite close to the actual x value.
The estimate for the velocity and acceleration remain unchanged and with large variances, since there is not enough data for them to update.. yet.
	)raw";

string noteCorrelations = R"raw(
We also see that the velocity terms have began to become corellated with their corresponding position terms.
	)raw";


string noteSecondMeas = R"raw(
After a second measurement the velocity terms begin to adjust to the measurements.
This takes place purely due to corellations with the position terms, caused by the propagation of the state transition matrix.
	)raw";

string noteThirdMeas = R"raw(
After a third measurement the acceleration term also begins to adjust to the measurements.
	)raw";

string note1Second = R"raw(
Over the next 1 second's worth of measurements, the velocities and acceleration terms have converged toward the correct values,
initially overshooting before oscillating around the correct value (a = -9.8)
The postfit residuals for most measurements have reduced significantly, despite the very poor initilisation conditions.
(The relatively good prefit residual for the num=1 measurement (the one with no velocity or acceleration integration)
is only due to the lack of motion at this point in time. The correct value is relatively close to the previous value, see below)
	)raw";

string note2Seconds = R"raw(
Over the next second's worth of measurements, the velocities and acceleration terms have further converged toward the correct values
Now however the position state with no velocity term has relatively poor residuals, due to the fast unmodelled kinematics, as compared with the state that is estimating and modelling a linear acceleration
	)raw";


void printWait(
	string str)
{
	std::cout << "\n\n\n\n\n" << str;
	std::cout << "(continue)\n";

	getline(std::cin, str);
	std::cout << "\n\n\n\n\n";
}

/** Ginan kalman filter basics
 * The basic structure for kalman filtering in ginan is the KFState struct, it is required for all filtering.
 * The KFKey struct is used to define a state element within the filter. These are used rather than row/column indexing to eliminate manual bookkeeping
 * State elements are generally initialised using an InitialState object, which defines the initial value, sigma, and process noise to be applied between epochs.
 * Measurements are defined individually as KFMeasEntry objects and added to a list, before being combined to create a KFMeas object that contains the aggregated measurement list in matrix form.
 * The measurement's design matrix is set by defining the sensitivity of the measurement to state elements, and its innovation (OMC) and noise are set with helper functions.
 * State elements are automatically added to the filter when a stateTransition occurs after referencing a KFKey with one of the helper functions.
 */
KFState walkthrough1()
{
	//unrequired preparation
	double actualPosition	= 10;
	double measVar			= 0.21;
	std::random_device					randoDev;
	std::mt19937						randoGen(randoDev());
	std::normal_distribution<double>	rando(0, SQRT(measVar));

	//prepare the filter
	KFState kfState;
	kfState.output_residuals = true;						//turn on nice outputs for pre/postfit residuals when filtering
	kfState.measRejectCallbacks.push_back(deweightMeas);	//turn on a rejection callback
	kfState.prefitOpts.	max_iterations	= 4;
	kfState.postfitOpts.max_iterations	= 4;

	//Prepare the initial state for whenever this element is eventaully initialised
	InitialState posInit;
	posInit.x = 0.0123;						//initial estimate of the state
	posInit.P = 900;						//initial variance of the state
	posInit.Q = 6;							//process noise per second

	GTime time;

																								printWait(noteIntro1);


	kfState.outputStates(std::cout, " - BEFORE ANYTHING HAPPENS");
																								printWait(noteONE);

	for (int epoch = 1; epoch <= 2; epoch++)
	{
		std::cout << "\n" << "Begin Epoch " << epoch << "\n";
		time += 60;

		KFMeasEntryList kfMeasEntryList;

		KFKey posKey;
		posKey.type	= KF::REC_POS;
		posKey.str	= "Walk";

																								if (epoch == 1)	printWait(noteAddMeas);
		{
			//get the computed (usually just current) state
			double stateVal = posInit.x;								//inital computed state is whatever we are going to initialise it as
			kfState.getKFValue(posKey, stateVal);						//override that with whatever is in the kalman filter, if there is something there, that is.

			double computed = stateVal;
			double observed = actualPosition + rando(randoDev);			//the measurement is the actual position +- some error noise

			double omc = observed - computed;

			KFMeasEntry measEntry(&kfState);
			measEntry.addDsgnEntry(posKey, 1, posInit);					//this measEntry is a direct measurement of the state with key posKey -> sensitivity is 1, create and initialise the state with posInit if required.
			measEntry.setInnov(omc);
			measEntry.setNoise(measVar);

			//this is optional, but makes the output easier to read. Usually this would be passed in through KFMeasEntry's constructor
			if (1)
			{
				KFKey obsKey;
				obsKey.str	= "Meas";
				obsKey.num	= 1337;
				measEntry.obsKey = obsKey;
			}

			kfMeasEntryList.push_back(measEntry);
		}

		{
			//get the computed (usually just current) state
			double stateVal = posInit.x;								//inital computed state is whatever we are going to initialise it as
			kfState.getKFValue(posKey, stateVal);						//override that with whatever is in the kalman filter, if there is something there, that is.

			double computed = stateVal / 2.54;							//this measurement is in inches, this time the 'computation' is needed before use.
			double observed = actualPosition / 2.54;					//the measurement is the actual position if it were measured in inches.

			if (epoch == 2)		observed += 14;

			double omc = observed - computed;

			KFMeasEntry measEntry(&kfState);
			measEntry.addDsgnEntry(posKey, 1 / 2.54, posInit);			//this measEntry is a scaled measurement of the state with key posKey -> sensitivity of measuremet to change in state is 1/2.54, create and initialise the state with posInit if required.
			measEntry.setInnov(omc); 									//error between the current state value, and what we think it should be (observed minus computed)
			measEntry.setNoise(measVar);								//how much variance we expect in the measurement - our rulers are not perfect

			//this is optional, but makes the output easier to read. Usually this would be passed in through KFMeasEntry's constructor
			if (1)
			{
				KFKey obsKey;
				obsKey.str	= "Inch";
				measEntry.obsKey = obsKey;
			}

			kfMeasEntryList.push_back(measEntry);
		}


		kfState.stateTransition(std::cout, time);					//the state transition to t = time initialises newly created states, and adds process noise to existing ones as per their initialisation

		kfState.outputStates(std::cout, " - POST STATE TRANSITION");
																						if (epoch == 1)	printWait(noteInit);
																						if (epoch == 2)	printWait(noteProcNoise);

		KFMeas kfMeas(kfState, kfMeasEntryList, time);		//filterKalman requires a consolidated KFMeas input, rather than a list of individial KFMeasEntrys, create it here

		if (1)
		{
			if (epoch == 1)
				kfState.outputMeasurements(std::cout, kfMeas);
																						if (epoch == 1)	printWait(noteMeas);
		}


		kfState.filterKalman(std::cout, kfMeas, "", true);		//perform the kalman filter operation (the true indicates this is an extended kalman filter using precalculated OMCs, this should generally be true in ginan)
																						if (epoch == 1)	printWait(noteResiduals);
																						if (epoch == 2) printWait(noteReject);


		kfState.outputStates(std::cout, " - POST FILTER");
																						if (epoch == 1)	printWait(notePostFilter1);
	}

	printWait("End of walkthrough section 1\n\t");

	return kfState;
}


void walkthrough2(
	KFState& kfState)
{
	//unrequired preparation
	double measVar			= 1;
	std::random_device					randoDev;
	std::mt19937						randoGen(randoDev());
	std::normal_distribution<double>	rando(0, SQRT(measVar));
	double v0	= 10;
	double x0	= 30;
	double a	= -9.8;
	double dt	= 0.02;
	GTime time = kfState.time + 1000;



	InitialState posInit;		//initial values for pos, vel, and acc.
	posInit.P = 10000;			//large P values indicate large uncertainty in initial position
								//initial x value defaults to 0 if undefined, substantially different from actual initial values defined above

	InitialState velInit;
	velInit.P = 10000;

	InitialState accInit;
	accInit.P = 10000;

																						printWait(noteIntro2);

	KFKey posKey;					//keys for identifying the states added to the filter
	posKey.type	= KF::POS;
	posKey.str	= "Obj";

	KFKey velKey;
	velKey.type	= KF::VEL;
	velKey.str	= "Obj";

	KFKey accKey;
	accKey.type	= KF::ACC;
	accKey.str	= "Obj";


	kfState.outputStates(std::cout, " - BEFORE ANYTHING HAPPENS");

																							printWait(noteRemove);

	//idiomatic procedure for iterating over things in the filter
	for (auto& [key, index] : kfState.kfIndexMap)
	{
		//dont remove ONE, we need that!
		if (key.type == KF::ONE)
		{
			continue;
		}

		//remove all other states
		kfState.removeState(key);
	}


	for (double t = 0; t <= 2; t += dt)
	{
		double actualPosition	= x0 							//compute positions and velocities of a simulated ball in gravity
								+ v0	* t
								+ a		* t * t / 2;

		double actualVelocity	= v0
								+ a		* t;

		std::cout << "\n" << "\n\n\n\nEpoch "
		<< "\n t = " << t
		<< "\n x = " << actualPosition
		<< "\n v = " << actualVelocity << "\n";
		time += dt;														//generally we wouldnt be iterating over t, dt like this, we would be given a time from some external observation source and we'd just use it

		KFMeasEntryList kfMeasEntryList;

		kfState.stateTransition(std::cout, time);						//since we're using rates we need to compute the current value of the states since they have changed over the period since the stateTransition at the end of this loop was last called, (at t-dt)
																		//call stateTransition to do this computation




		if (t == 0)
			kfState.outputStates(std::cout, " - POST FIRST STATE TRANSITION");
																							if (t == 0)	printWait(noteAddMeas + "\nAlso note that the current true value of the x,v,t at each epoch will be shown as above\n\t");

		double observed = actualPosition;				//the measurement is the actual position

		//create three types of measurements for the same object, with more or less kinematics involved
		for (int i = 1; i <= 3; i++)
		{
			posKey.num = i;
			velKey.num = i;
			accKey.num = i;

			if		(i == 1)	{		posInit.Q = SQR(10);							}
			else if (i == 2)	{		posInit.Q = 0;			velInit.Q = SQR(10);	}	//The Q value (process noise) should be 0 for elements that have rates applied - eg, for num==2 a vel term is the derivative of pos, so posInit.Q = 0
			else if (i == 3)	{		posInit.Q = 0;			velInit.Q = 0;			}	//only the highest derivative should get process noise.

			//get the computed (usually just current) state
			double stateVal = posInit.x;								//inital computed state is whatever we are going to initialise it as
			kfState.getKFValue(posKey, stateVal);						//override that with whatever is in the kalman filter, if there is something there, that is.

			double computed = stateVal;

			double omc = observed - computed;

			KFKey obsKey;			//give the measEntry a KFKey just so the residuals/measurements outputs are easier to read (optional)
			obsKey.str = "MEAS";
			obsKey.num = i;

			KFMeasEntry measEntry(&kfState, obsKey);
			measEntry.addDsgnEntry(posKey, 1, posInit);		//this measEntry is a direct measurement of the state with key posKey -> sensitivity is 1, create and initialise the state with posInit if required.
			measEntry.setInnov(omc);
			measEntry.setNoise(measVar);

			if (i >= 2)
				kfState.setKFTransRate(posKey, velKey,			1, velInit);		//here we define that velKey is the rate derivative of posKey, with scaling 1, and some initial velocity state

			if (i == 3)
				kfState.setAccelerator(posKey, velKey, accKey,	1, accInit);		//here we define that accKey is an accelerator of posKey, with velKey being an intermediate derivative between that and this.


			kfMeasEntryList.push_back(measEntry);
		}


		kfState.stateTransition(std::cout, time);			//since we called stateTransition with t = time above, this will only initialise new states - no time has passed to change state values or process noises

		if (t < dt*3)
			kfState.outputStates(std::cout, " - POST SECOND STATE TRANSITION");


		KFMeas kfMeas(kfState, kfMeasEntryList, time);


		if (1)
		{
			if (t == 0)
				kfState.outputMeasurements(std::cout, kfMeas);
																						if (t == 0)	printWait(noteMeas2);


			if (t == dt)
				kfState.outputCorrelations(std::cout);
																						if (t == dt)	printWait(noteCorrelations);
		}


		kfState.filterKalman(std::cout, kfMeas, "", true);


		kfState.outputStates(std::cout, " - POST FILTER");
																						if (t == 0)			printWait(noteFirstMeas);
																						if (t == dt)		printWait(noteSecondMeas);
																						if (t == dt*2)		printWait(noteThirdMeas);
																						if (t > 1)			printWait(note1Second);
	}

	printWait(note2Seconds);
	printWait("End of walkthrough section 2");
}

void walkthrough()
{
	KFState kfState =	walkthrough1();
						walkthrough2(kfState);
	exit(1);
}
