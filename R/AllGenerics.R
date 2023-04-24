# GENERIC METHODS
#' @include AllClasses.R
NULL

# Tools ========================================================================
## Mutators --------------------------------------------------------------------
#' Get or Set Parts of an Object
#'
#' Getters and setters to extract or replace parts of an object.
#' @param x An object from which to get or set element(s).
#' @param value A possible value for the element(s) of `x`.
#' @return
#'  An object of the same sort as `x` with the new values assigned.
# @example inst/examples/ex-mutator.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name mutators
#' @rdname mutators
#' @aliases get set
NULL

## Subset ----------------------------------------------------------------------
#' Extract or Replace Parts of an Object
#'
#' Operators acting on objects to extract or replace parts.
#' @param x An object from which to extract element(s) or in which to replace
#'  element(s).
#' @param i,j Indices specifying elements to extract or replace.
#' @param drop A [`logical`] scalar: should the result be coerced to
#'  the lowest possible dimension? This only works for extracting elements,
#'  not for the replacement.
# @param value A possible value for the element(s) of `x`.
#' @param ... Currently not used.
#' @return
#'  A subsetted object.
# @example inst/examples/ex-subset.R
#' @author N. Frerebeau
#' @docType methods
#' @family mutators
#' @name subset
#' @rdname subset
NULL

# Time Scales ==================================================================
#' Time Scales
#'
#' @param object A [`character`] string specifying the time scale (see details).
#' @param ... Currently not used.
#' @details
#'  The following time scales are available:
#'  \describe{
#'   \item{`BP`}{Before Present.}
#'   \item{`BC`}{Before Christ.}
#'   \item{`BCE`}{Before Common Era.}
#'   \item{`AD`}{Anno Domini.}
#'   \item{`CE`}{Common Era.}
#'   \item{`b2k`}{Years before 2000.}
#'  }
#' @return
#'  A [`TimeScale-class`] object.
#' @note
#'  Inspired by [era::era()] by Joe Roe.
#' @example inst/examples/ex-era.R
#' @author N. Frerebeau
#' @docType methods
#' @family time scales
#' @aliases era-method
#' @keywords internal
setGeneric(
  name = "era",
  def = function(object, ...) standardGeneric("era")
)

#' Calendar Converter
#'
#' Interconverts dates in a variety of calendars.
#' @param from A [`TimeScale-class`] object describing the source era.
#' @param to A [`TimeScale-class`] object describing the target era.
#' @param ... Currently not used.
#' @return
#'  A [`function`] that when called with a single numeric argument (years)
#'  converts years from one calendar to another.
#' @note
#'  Adapted from [era::yr_transform()] by Joe Roe.
#' @example inst/examples/ex-era.R
#' @author N. Frerebeau
#' @docType methods
#' @family time scales
#' @aliases convert-method
#' @keywords internal
setGeneric(
  name = "convert",
  def = function(from, to, ...) standardGeneric("convert")
)

#' Change the Time Scale
#'
#' Change the time scale of an object.
#' @param object An object describing the source era.
#' @param target A [`character`] string specifying the target time scale
#'  (see [era()]).
#' @param ... Currently not used.
#' @return
#'  An object.
#' @author N. Frerebeau
#' @docType methods
#' @family time scales
#' @aliases project-method
setGeneric(
  name = "project",
  def = function(object, target, ...) standardGeneric("project")
)

#' Time Grid
#'
#' Get the time grid of an object.
#' @param object An object.
#' @return
#'  A [`numeric`] vector.
#' @author N. Frerebeau
#' @docType methods
#' @family time scales
#' @aliases years-method
setGeneric(
  name = "years",
  def = function(object) standardGeneric("years")
)

# Radiocarbon ==================================================================
## Calibration curve -----------------------------------------------------------
#' 14C Calibration Curve
#'
#' @param x A [`character`] string naming a calibration curve (see details).
#' @param ... Currently not used.
#' @details
#'  The following calibration curves are available:
#'
#' \tabular{ll}{
#'  **Curve**    \tab **Reference** \cr
#'  `bomb04NH1`  \tab Hua and Barbetti 2004 \cr
#'  `bomb04NH2`  \tab Hua and Barbetti 2004 \cr
#'  `bomb04NH3`  \tab Hua and Barbetti 2004 \cr
#'  `bomb04SH`   \tab Hua and Barbetti 2004 \cr
#'  `bomb13nh1`  \tab Hua, Berbetti and Rakowski 2013 \cr
#'  `bomb13nh2`  \tab Hua, Berbetti and Rakowski 2013 \cr
#'  `bomb13nh3`  \tab Hua, Berbetti and Rakowski 2013 \cr
#'  `bomb13sh12` \tab Hua, Berbetti and Rakowski 2013 \cr
#'  `bomb13sh3`  \tab Hua, Berbetti and Rakowski 2013 \cr
#'  `bomb21nh1`  \tab Hua et al. 2022 \cr
#'  `bomb21nh2`  \tab Hua et al. 2022 \cr
#'  `bomb21nh3`  \tab Hua et al. 2022 \cr
#'  `bomb21sh12` \tab Hua et al. 2022 \cr
#'  `bomb21sh3`  \tab Hua et al. 2022 \cr
#'  `cariaco04`  \tab Hughen et al. 2004 \cr
#'  `intcal04`   \tab Reimer et al. 2004 \cr
#'  `intcal09`   \tab Reimer et al. 2009 \cr
#'  `intcal13`   \tab Reimer et al. 2013 \cr
#'  `intcal20`   \tab Reimer et al. 2020 \cr
#'  `intcal98`   \tab Stuiver et al. 1998 \cr
#'  `Kueppers04` \tab Kueppers et al. 2004 \cr
#'  `marine04`   \tab Hughen et al. 2004 \cr
#'  `marine09`   \tab Reimer et al. 2009 \cr
#'  `marine13`   \tab Reimer et al. 2013 \cr
#'  `marine20`   \tab Heaton et al. 2020 \cr
#'  `marine98`   \tab Stuiver, Reimer and Braziunas 1998 \cr
#'  `shcal04`    \tab McCormac et al. 2004 \cr
#'  `shcal13`    \tab Hogg et al. 2013 \cr
#'  `shcal20`    \tab Hogg et al. 2020 \cr
#' }
#'
#' @return
#'  A three-column [`data.frame`]: calibrated age BP, uncalibrated age BP and
#'  standard deviation.
#' @references
#'  Heaton, Timothy J, Peter Köhler, Martin Butzin, Edouard Bard, Ron W Reimer,
#'  William E N Austin, Christopher Bronk Ramsey, et al. (2020). Marine20 The
#'  Marine Radiocarbon Age Calibration Curve (0-55,000 Cal BP).
#'  *Radiocarbon*, 62(4): 779-820. \doi{10.1017/RDC.2020.68}.
#'
#'  Hogg, Alan G, Timothy J Heaton, Quan Hua, Jonathan G Palmer, Chris SM
#'  Turney, John Southon, Alex Bayliss, et al. (2020). SHCal20 Southern
#'  Hemisphere Calibration, 0-55,000 Years Cal BP. *Radiocarbon*, 62(4): 759-78.
#'  \doi{10.1017/RDC.2020.59}.
#'
#'  Hogg, Alan G, Quan Hua, Paul G Blackwell, Mu Niu, Caitlin E Buck, Thomas P
#'  Guilderson, Timothy J Heaton, et al. (2013). SHCal13 Southern Hemisphere
#'  Calibration, 0-50,000 Years Cal BP. *Radiocarbon*, 55(4): 1889-1903.
#'  \doi{10.2458/azu_js_rc.55.16783}.
#'
#'  Hua, Quan, and Mike Barbetti (2004). Review of Tropospheric Bomb 14C Data
#'  for Carbon Cycle Modeling and Age Calibration Purposes. *Radiocarbon*,
#'  46(3): 1273-1298. \doi{10.1017/S0033822200033142}.
#'
#'  Hua, Quan, Mike Barbetti, and Andrzej Z Rakowski (2013). Atmospheric
#'  Radiocarbon for the Period 1950-2010. *Radiocarbon*, 55(4): 2059‑2072.
#'  \doi{10.2458/azu_js_rc.v55i2.16177}.
#'
#'  Hua, Quan, Jocelyn C Turnbull, Guaciara M Santos, Andrzej Z Rakowski,
#'  Santiago Ancapichún, Ricardo De Pol-Holz, Samuel Hammer, et al. (2022).
#'  Atmospheric Radiocarbon for the Period 1950-2019. *Radiocarbon*,
#'  64(4): 723‑745. \doi{10.1017/RDC.2021.95}.
#'
#'  Hughen, K., S. Lehman, J. Southon, J. Overpeck, O. Marchal, C. Herring,
#'  and J. Turnbull (2004). 14C Activity and Global Carbon Cycle Changes over
#'  the Past 50,000 Years. *Science*, 303(5655): 202‑207.
#'  \doi{10.1126/science.1090300}.
#'
#'  Hughen, Konrad A, Mike G L Baillie, Edouard Bard, J Warren Beck, Chanda J H
#'  Bertrand, Paul G Blackwell, Caitlin E Buck, et al. (2004). Marine04 Marine
#'  Radiocarbon Age Calibration, 0-26 cal kyr BP. *Radiocarbon*,
#'  46(3): 1059‑1086. \doi{10.1017/S0033822200033002}.
#'
#'  Kueppers, Lara M., John Southon, Paul Baer, and John Harte (2004). Dead Wood
#'  Biomass and Turnover Time, Measured by Radiocarbon, along a Subalpine
#'  Elevation Gradient. *Oecologia*, 141(4): 641‑651.
#'  \doi{10.1007/s00442-004-1689-x}.
#'
#'  McCormac, F G, A G Hogg, P G Blackwell, C E Buck, T F G Higham, and P J
#'  Reimer (2004). Shcal04 Southern Hemisphere Calibration, 0-11.0 cal kyr BP.
#'  *Radiocarbon*, 46(3): 1087‑1092. \doi{10.1017/S0033822200033014}.
#'
#'  Reimer, P J, M G L Baillie, E Bard, A Bayliss, J W Beck, P G Blackwell,
#'  C Bronk Ramsey, et al. (2009). IntCal09 and Marine09 Radiocarbon Age
#'  Calibration Curves, 0-50,000 Years cal BP. *Radiocarbon*, 51(4): 1111‑1150.
#'  \doi{10.1017/S0033822200034202}.
#'
#'  Reimer, Paula J, William E N Austin, Edouard Bard, Alex Bayliss, Paul G
#'  Blackwell, Christopher Bronk Ramsey, Martin Butzin, et al. (2020).
#'  The IntCal20 Northern Hemisphere Radiocarbon Age Calibration Curve
#'  (0-55 cal kBP). *Radiocarbon*, 62(4): 725‑757. \doi{10.1017/RDC.2020.41}.
#'
#'  Reimer, Paula J, Mike G L Baillie, Edouard Bard, Alex Bayliss,
#'  J Warren Beck, Chanda J H Bertrand, Paul G Blackwell, et al. (2004).
#'  Intcal04 Terrestrial Radiocarbon Age Calibration, 0-26 cal kyr BP.
#'  *Radiocarbon*, 46(3): 1029‑1058. \doi{10.1017/S0033822200032999}.
#'
#'  Reimer, Paula J, Edouard Bard, Alex Bayliss, J Warren Beck, Paul G
#'  Blackwell, Christopher Bronk Ramsey, Caitlin E Buck, et al. (2013).
#'  IntCal13 and Marine13 Radiocarbon Age Calibration Curves 0-50,000
#'  Years cal BP. *Radiocarbon*, 55(4): 1869‑1887.
#'  \doi{10.2458/azu_js_rc.55.16947}.
#'
#'  Stuiver, Minze, Paula J. Reimer, Edouard Bard, J. Warren Beck, G. S. Burr,
#'  Konrad A. Hughen, Bernd Kromer, Gerry McCormac, Johannes Van Der Plicht, and
#'  Marco Spurk (1998). INTCAL98 Radiocarbon Age Calibration, 24,000-0 cal BP.
#'  *Radiocarbon*, 40(3): 1041‑1083. \doi{10.1017/S0033822200019123}.
#'
#'  Stuiver, Minze, Paula J. Reimer, and Thomas F. Braziunas. (1998).
#'  High-Precision Radiocarbon Age Calibration for Terrestrial and Marine
#'  Samples. *Radiocarbon*, 40(3): 1127‑1151. \doi{10.1017/S0033822200019172}.
#' @example inst/examples/ex-14c-curve.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases c14_curve-method
setGeneric(
  name = "c14_curve",
  def = function(x, ...) standardGeneric("c14_curve"),
  valueClass = "data.frame"
)

## Calibration -----------------------------------------------------------------
#' 14C Calibration
#'
#' Calibrates radiocarbon dates.
#' @param ages A [`numeric`] vector giving the BP ages to be calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the ages
#'  to be calibrated.
#' @param names A [`character`] vector specifying the names of the ages (e.g.
#'  laboratory codes).
#' @param curves A [`character`] vector specifying the calibration curve to be
#'  used. Different curves can be specified per dated sample.
#' @param reservoir_offsets A [`numeric`] vector giving the offset values for
#'  any marine reservoir effect (defaults to 0; i.e. no offset).
#' @param reservoir_errors A [`numeric`] vector giving the offset value errors
#'  for any marine reservoir effect (defaults to 0; i.e. no offset).
#' @param from length-one [`numeric`] vector specifying the earliest data to
#'  calibrate for, in cal. BP years.
#' @param to A length-one [`numeric`] vector specifying the latest data to
#'  calibrate for, in cal. BP years.
#' @param resolution A length-one [`numeric`] vector specifying the temporal
#'  resolution (in years) of the calibration.
#' @param normalize A [`logical`] scalar: should the calibration be normalized?
#' @param F14C A [`logical`] scalar: should the calibration be carried out in
#'  F14C space?
#' @param drop A [`logical`] scalar: should years with zero probability be
#'  discarded? If `TRUE` (the default), results in a narrower time range.
#' @param eps A length-one [`numeric`] value giving the cutoff below which
#'  calibration values will be removed.
#' @param ... Currently not used.
#' @return
#'  A [`CalibratedAges-class`] object.
#' @references
#'  Bronk Ramsey, C. (2008). RADIOCARBON DATING: REVOLUTIONS IN UNDERSTANDING.
#'  *Archaeometry*, 50:249-275. \doi{10.1111/j.1475-4754.2008.00394.x}.
#' @note
#'  Adapted from [rcarbon::calibrate()] by Andrew Bevan and Enrico Crema.
#' @example inst/examples/ex-14c-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases c14_calibrate-method
setGeneric(
  name = "c14_calibrate",
  def = function(ages, errors, ...) standardGeneric("c14_calibrate"),
  valueClass = "CalibratedAges"
)

#' F14C
#'
#' Converts F14C ratio to 14C age.
#' @param ages A [`numeric`] vector giving the radiocarbon ages.
#' @param ratios A [`numeric`] vector giving the F14C ratios.
#' @param errors A [`numeric`] vector giving the standard deviation of the
#'  ages/ratios.
#' @param ... Currently not used.
#' @return
#'  A two-column [`data.frame`].
#' @references
#'  Bronk Ramsey, C. (2008). RADIOCARBON DATING: REVOLUTIONS IN UNDERSTANDING.
#'  *Archaeometry*, 50:249-275. \doi{10.1111/j.1475-4754.2008.00394.x}.
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @name F14C
#' @rdname F14C
NULL

#' @rdname F14C
#' @aliases BP14C_to_F14C-method
setGeneric(
  name = "BP14C_to_F14C",
  def = function(ages, errors, ...) standardGeneric("BP14C_to_F14C"),
  valueClass = "data.frame"
)

#' @rdname F14C
#' @aliases F14C_to_BP14C-method
setGeneric(
  name = "F14C_to_BP14C",
  def = function(ratios, errors, ...) standardGeneric("F14C_to_BP14C"),
  valueClass = "data.frame"
)

## Combine ---------------------------------------------------------------------
#' 14C Calibration
#'
#' Combines radiocarbon dates.
#' @param ages A [`numeric`] vector giving the BP ages to be calibrated.
#' @param errors A [`numeric`] vector giving the standard deviation of the ages
#'  to be calibrated.
#' @param groups A [`factor`] in the sense that `as.factor(groups)` defines the
#'  the groups to combine with. If `NULL` (the default), all dates are combined.
#'  `NA`s will be treated as isolated dates.
#' @param ... Currently not used.
#' @return
#'  A [`data.frame`] with the following columns:
#'  \tabular{ll}{
#'  `groups` \tab Group names                      \cr
#'  `ages`   \tab Combined 14C ages                \cr
#'  `errors` \tab Combined 14C standard deviations \cr
#'  `chi2`   \tab Chi-squared test statistic       \cr
#'  `p`      \tab Chi-squared test p-value         \cr
#'  }
#' @references
#'  Ward, G. K. and Wilson, S. R. (1978). Procedures for Comparing and Combining
#'  Radiocarbon Age Determinations: A Critique. *Archaeometry* 20(1): 19‑31.
#'  \doi{10.1111/j.1475-4754.1978.tb00208.x}.
#' @example inst/examples/ex-14c-combine.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases c14_combine-method
setGeneric(
  name = "c14_combine",
  def = function(ages, errors, ...) standardGeneric("c14_combine"),
  valueClass = "data.frame"
)

## HPDI ------------------------------------------------------------------------
#' HPD Regions
#'
#' @param object A [`numeric`] vector giving the coordinates of the points where
#'  the density is estimated or a [`CalibratedAges-class`] object.
#' @param density A [`numeric`] vector giving the estimated density values.
#'  If `density` is missing and `object` is a `ǹumeric` vector, density
#'  estimates will be computed from `object`.
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param ... Currently not used.
#' @return
#'  A [`list`] of `numeric` [`matrix`] giving the lower and upper boundaries of
#'  the HPD interval and associated probabilities.
#' @references
#'  Hyndman, R. J. (1996). Computing and graphing highest density regions.
#'  *American Statistician*, 50: 120-126. \doi{10.2307/2684423}.
#' @example inst/examples/ex-14c-hpdi.R
#' @author N. Frerebeau
#' @family statistics
#' @docType methods
#' @aliases hpdi-method
setGeneric(
  name = "hpdi",
  def = function(object, density, ...) standardGeneric("hpdi")
)

## Plot ------------------------------------------------------------------------
#' Plot Calibrated Radiocarbon Ages
#'
#' @param x A [`CalibratedAges-class`] or [`CalibratedSPD-class`] object.
#' @param density A [`logical`] scalar: should density be drawn?
#' @param interval A [`logical`] scalar: should highest posterior density
#'  interval be drawn?
#' @param level A length-one [`numeric`] vector giving the confidence level.
#' @param warnings A [`logical`] scalar: should warnings be plotted?
#' @param decreasing A [`logical`] scalar: should the sort order be decreasing?
#' @param xlim A length-two [`numeric`] vector giving the x limits of the plot.
#' @param ylim A length-two [`numeric`] vector giving the y limits of the plot.
#' @param main A [`character`] string giving a main title for the plot.
#' @param sub A [`character`] string giving a subtitle for the plot.
#' @param ann A [`logical`] scalar: should the default annotation (title and x,
#'  y and z axis labels) appear on the plot?
#' @param axes A [`logical`] scalar: should axes be drawn on the plot?
#' @param frame.plot A [`logical`] scalar: should a box be drawn around the
#'  plot?
#' @param panel.first An an `expression` to be evaluated after the plot axes are
#'  set up but before any plotting takes place. This can be useful for drawing
#'  background grids.
#' @param panel.last An `expression` to be evaluated after plotting has taken
#'  place but before the axes, title and box are added.
#' @param ... Other [graphical parameters][graphics::par] may also be passed as
#'  arguments to this function.
#' @return
#'  `plot()` is called it for its side-effects: it results in a graphic
#'  being displayed. Invisibly returns `x`.
#' @example inst/examples/ex-14c-calibrate.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @name c14_plot
#' @rdname c14_plot
NULL

## SPD -------------------------------------------------------------------------
#' Summed Probability Distributions
#'
#' Computes summed probability distributions (SPD) of radiocarbon dates.
#' @param object A [`CalibratedAges-class`] object.
#' @param normalize_date A [`logical`] scalar: should the total probability mass
#'  of the calibrated dates be normalised (to sum to unity within the time-span
#'  of analysis)?
#' @param normalize_spd A [`logical`] scalar: should the total probability mass
#'  of the SPD be normalised (to sum to unity)?
#' @param ... Currently not used.
#' @example inst/examples/ex-14c-spd.R
#' @author N. Frerebeau
#' @docType methods
#' @family radiocarbon tools
#' @aliases spd-method
setGeneric(
  name = "spd",
  def = function(object, ...) standardGeneric("spd"),
  valueClass = "CalibratedSPD"
)

# Interval Analysis ============================================================
## Allen Interval Algebra ------------------------------------------------------
#' Allen Relation Between Definite Intervals
#'
#' @param x,y A [`numeric`] vector giving the lower and upper boundaries of the
#'  time intervals, respectively. If `y` is missing, an attempt is made to
#'  interpret `x` in a suitable way (see [grDevices::xy.coords()]).
#' @param ... Currently not used.
#' @details
#'
#' \tabular{lrlr}{
#'  **Relation** \tab     \tab     \tab  **Converse** \cr
#'  precedes     \tab (p) \tab (P) \tab   preceded by \cr
#'  meets        \tab (m) \tab (M) \tab        met by \cr
#'  overlaps     \tab (o) \tab (O) \tab overlapped by \cr
#'  finished by  \tab (F) \tab (f) \tab      finishes \cr
#'  contains     \tab (D) \tab (d) \tab        during \cr
#'  starts       \tab (s) \tab (S) \tab    started by \cr
#'  equals       \tab (e) \tab     \tab               \cr
#' }
#'
#' @return
#'  A [`character`] matrix specifying the Allen relations.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#'
#'  Alspaugh, T. (2019). Allen's Interval Algebra.
#'  URL: \url{https://thomasalspaugh.org/pub/fnd/allen.html}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_relation-method
setGeneric(
  name = "allen_relation",
  def = function(x, y, ...) standardGeneric("allen_relation")
)

#' Complement of an Allen Relation
#'
#' @param x A [`character`] vector or matrix of Allen relations (typically
#'  returned by [allen_relation()]).
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector or matrix (same as `x`).
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_complement-method
setGeneric(
  name = "allen_complement",
  def = function(x, ...) standardGeneric("allen_complement")
)

#' Converse of an Allen Relation
#'
#' @param x A [`character`] vector or matrix of Allen relations (typically
#'  returned by [allen_relation()]).
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector or matrix (same as `x`).
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_converse-method
setGeneric(
  name = "allen_converse",
  def = function(x, ...) standardGeneric("allen_converse")
)

#' Composition of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_composition-method
setGeneric(
  name = "allen_composition",
  def = function(x, y, ...) standardGeneric("allen_composition")
)

#' Intersection of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_intersect-method
setGeneric(
  name = "allen_intersect",
  def = function(x, y, ...) standardGeneric("allen_intersect")
)

#' Union of Allen Relations
#'
#' @param x,y A [`character`] vector of Allen relations.
#' @param ... Currently not used.
#' @return
#'  A [`character`] vector.
#' @references
#'  Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
#'  *Communications of the ACM*, 26(11): 832-843. \doi{10.1145/182.358434}.
#' @example inst/examples/ex-allen.R
#' @author N. Frerebeau
#' @family Allen's intervals
#' @docType methods
#' @aliases allen_union-method
setGeneric(
  name = "allen_union",
  def = function(x, y, ...) standardGeneric("allen_union")
)

# Statistics ===================================================================
#' Mean
#'
#' @param x A [`CalibratedAges-class`] object.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param ... Currently not used.
#' @example inst/examples/ex-14c-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name mean
#' @rdname mean
NULL

#' Median
#'
#' @param x A [`CalibratedAges-class`] object.
#' @param na.rm A [`logical`] scalar: should `NA` values be stripped before the
#'  computation proceeds?
#' @param ... Currently not used.
#' @example inst/examples/ex-14c-statistics.R
#' @author N. Frerebeau
#' @docType methods
#' @family statistics
#' @name median
#' @rdname median
NULL
