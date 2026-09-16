
# Rauch–Tung–Striebel (RTS) Smoothing

While the Kalman filter is an optimal solution to computing state estimates from all previous data, better estimates could be obtained if all future data were also incorporated.

The RTS Smoothing algorithm is an approach to determine estimates of states and uncertainties by considering the state transition between two Kalman filtered estimates, smoothing the transition between prior and 'future' data.

All Kalman filters in the toolkit are capable of having RTS Smoothing applied. If configured appropriately with an RTS\_lag and output files, intermediate filter results will be stored to file for reverse smoothing.

For real-time processing, a small lag may be applied to improve short-term accuracy. After each filtering stage, the new result is propagated backward through time to correct the previous N epochs. Each epoch worth of lag however requires a comparable processing time to an Kalman filter processing stage - a lag of N epochs may slow processing by up to a multiple of N.

For post-processing, the optimal lag is to use all future and past data. This is achieved by first computing the forward solution, before propagating the final results backward through to the first epoch. The processing time required for a complete backward smoothed filter may be less than 2x a non-smoothed filter - considerably faster than a finite lag in real-time.

## Example

Consider the system of a single particle moving in one dimension. At time t=0, the particle's position is measured to be x=0. The system then evolves with a random walk, with no more measurements until the time t=100.

At t=99, the particles position has a large uncertainty - it has likely moved from its original position, however, with no more data available, its mean expected value remains as x=0.

At t=100, the particles position is again measured, this time as x=10. If this system were monitored using a Kalman filter, the expected position of the particle would be a constant x=0 for the first 99 seconds, before an abrupt change in location at t=100. During the 100 seconds, the variance would increase steadily, before abruptly returning to a low value when the second measurement is taken.

If the Kalman filter measurements were taken in reverse order, with the first measurement at t=100, the variance of the particle would steadily increase going backward in time until t=0, before the second measurement again reduced the variance, this time at t=0.

Using an RTS Smoother effectively combines both forward and backward filtering. At t=1, the variance is low due to the measurement at t=0. Likewise, at t=99, the variance is low due to the measurement at t=100. In this example, in addition to the measurements at t=0 and t=100, the expected mean value at t=50 can be expected to be the midpoint between the two measurements - a result that can not be obtained using Kalman filtering alone.

In order to accurately calculate the expected position and variance at t=50 however, knowledge of the measurement at t=100 was required (50 seconds later). RTS smoothed estimates necessarily lag behind the primary Kalman filter to allow some time for future data to be obtained. The length of this lag determines the effectiveness of the smoothing.

 
