
# Minimum Constraints

When running the toolkit in network mode, the positions of receivers and satellites may be estimated simultaneously. This presents a complication in that the absolute positions of all elements may be unconstrained - the model of the system would be completely consistent if every element of the network (receivers and satellites) were on the opposite side of the planet! For the results of such processing to be of value, the system needs to be referenced to a standard reference frame.

One method of ensuring the system is referenced to a suitable reference frame is to constrain receiver positions to their nominal position in the reference frame. Constraining 3 receivers is sufficient to ensure the system is well defined, but this gives select receivers precedence over all others - a movement of one of these receivers will instead show up as a movement of every other receiver on the planet.

An alternative method may be to weakly constrain all receivers to their nominal positions. This removes the priority effect of choosing 3 receivers, but will also apply a small restoring bias to the estimated position of the receivers.

Minimum constrains is a method of referring a system of estimates to a standard reference frame without unduly prioritising any receiver, or biasing the measurements.

## Computation and Application

To compute a minimally constrained system, the deviations of all receivers from their nominal positions are used as pseudo-observations in a filter to estimate a transformation between reference frames.

The filter estimates a rigid transformation comprising of 3d rotation and translation components, which when applied produces a least-squares solution of the errors of all desired receivers.

The estimated transformation is then applied to the network solution, transforming both the estimated position states, as well as the covariances associated with them.
