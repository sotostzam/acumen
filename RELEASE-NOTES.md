# Release Notes

## Acumen 2016_08_30

- Improvements to the 2015 semantics:
  - Added support for equations
  - Added support for symbolic time and partial differentiation
  - New command line input feature
  - New print operation
  - New _plot support for conveniently plotting variables
- New aspect ratio support
- New 3D object Triangle
- Improvement to the transparency parameter for 3D object
- Improvement to the static view of 3D

## ACUMEN 2016_03_22

- Improvements to the 2015 Enclosure semantics
  - New integrator based on Taylor series integration and Lohner’s
    method, with better accuracy on non-linear systems.
  - Adaptive time stepping with event localisation. Enabled by
    setting simulator.minTimeStep to a small value such as 1/2^30.
- New transparency parameter for 3D object
- New coordinates parameter for keeping 3D object from manual rotation

## ACUMEN 2015_11_01

### Major changes since the 2014_12_01 release

- New Stream Animation mode shows _3D animation during simulation
- New Device input feature
  - Use a mobile device (such as a smartphone) to control objects in Acumen
- New matrix operations
- New notation for conveniently generating vectors
- Improved support for pattern matching
- Continuous assignments are always taken into account when
   expressions are evaluated
- Improved interval arithmetic based on the JInterval library

### Major changes since the 2014_11_03 release

- Improved stability of _3D view
- Re-enabled optimized traditional semantics
- Stricter error checking
  - All duplicate assignments (of matching kind) are treated as errors
     In particular, now parent assignments are also checked
  - Only discrete assignments to simulator parameters are allowed

### Major changes since the Acumen November 2013 release

- IMPORTANT:  Older semantics are temporarily disabled
- Change of license to BSD
- Language enhancements
  - Lighter and more expressive syntax
    - More powerful pattern "match" statement replaces "switch"
  - New "hypothesis" statement for specifying properties of systems, to be checked during simulation
- New implementation of _3D functionality
  - Based on a software renderer that eliminates need to install third-party libraries
  - Full compatibility on all platforms (Windows, Linux and Mac OS) and Java versions from 6 to 8
- Improved plotter
  - Now supports plotting of string-valued variables
- Improved default ("2014 Reference") semantics
  - Integration using Runge-Kutta-based solver is now default,
     yielding better precision for models with complex continuous
     dynamics
  - Both continuous and discrete assignments are performed
     simultaneously
- New semantics ("2014 Enclosure")
  - Supports a larger subset of the Acumen language, including
     models comprising multiple objects, nesting of if and match
     statements, and continuous assignments to unprimed variables

### Major changes since the Acumen 2012 preview release (Dec 2012)

- Improved default ("Traditional") semantics
  - Repeated assignments to the same variable no longer allowed, and
     now cause simulation fails with an error
  - All discrete assignments are performed simultaneously
  - Derivatives are interpreted in a more direct manner than before
     NOTE: As these semantics changes reject or alter the behavior of
     some old models the original semantics found in Acumen 2012 will
     available under under Semantics as "Traditional 2012" in future
     releases.
- A new and integrated user reference manual is found under Help
- User interface enhancements
  - Most notably:  File browser, code search, and code completion
- Improvements to memory utilization
- Language enhancements
  - A "let" operator has been added to expressions
  - Class names can now be used as values
  - Vector indexing is now supported
- Enhanced enclosure semantics
  - Transcendental functions ("sin" and "cos")
  - Extended predicate language
  - Improved event localization algorithm
  - New event handling algorithm
- Various other minor enhancements and fixes

Major changes since Acumen 2010:

- _3D support based on Yingfu Zeng's Masters thesis
- Enclosure semantics with support for Zeno systems
- A more responsive user interface
- Some improvements to syntax
  - Continuous assignment used to be [=] and is now just =
  - Discrete assignment used to be = and is now :=
- Syntax highlighting
