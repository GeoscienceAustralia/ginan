> **19 Aug 2026** - the Ginan team is pleased to release Ginan patch v4.1.3
>
> **Highlights**:
>
> * Fixed RINEX time parsing when `str2time` reads overflowing fixed-width fields
> * Added configurable preprocessor frequency selection for more flexible code/frequency priorities
> * Updated broadcast ephemeris IODE selection to support use of the latest suitable ephemeris/IODE in real-time processing
> * Added a safeguard to wait at least 30 seconds before changing IODE when uploading SSR streams
> * Added a Doppler placeholder path in the preprocessor to support upcoming Doppler-based preprocessing work
> * The binaries can be found on the GitHub website:
>   * [Ginan v4.1.3 Binaries](https://github.com/GeoscienceAustralia/ginan/releases/tag/v4.1.3)
