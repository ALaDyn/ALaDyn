# ALaDyn

[![ALaDyn Continuous Integration](https://github.com/ALaDyn/ALaDyn/actions/workflows/ccpp.yml/badge.svg)](https://github.com/ALaDyn/ALaDyn/actions/workflows/ccpp.yml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.592388.svg)](http://dx.doi.org/10.5281/zenodo.592388)
![GitHub tag (latest SemVer)](https://img.shields.io/github/tag/aladyn/aladyn.svg?label=Latest%20Release)

![ALaDyn Logo](https://raw.githubusercontent.com/ALaDyn/ALaDyn/master/media/logo.png)

`ALaDyn` (**A**cceleration by **La**ser and **Dyn**amics of charged particles) is a PIC code firstly described in *ALaDyn: A High-Accuracy PIC code for the Maxwell-Vlasov Equations* by C. Benedetti et al., published on IEEE Transactions on Plasma Science, **36** 4, 1790-1798 (2008) and then again in the update *Charge preserving high order PIC schemes* by P. Londrillo et al., published on Nucl. Instrum. Meth. A, **620** 1, 28-35 (2010). PWFA modules have been presented in *Numerical investigation of beam-driven PWFA in quasi-nonlinear regime* by P. Londrillo et al., published on Nucl. Instrum. Meth. A, **740** (2014).

To date, main usage of the code has been carried on in simulating:

- **L**aser **W**ake**F**ield **A**cceleration (**LWFA** Laser driven),

- **P**lasma **W**ake**F**ield **A**cceleration (**PWFA** Beam driven),

- **T**arget **N**ormal **S**heath **A**cceleration (**TNSA** Interaction with solid targets).

This newer version, in part rewritten from scratch, is released as is, without any warranty, and will be maintained here on GitHub. A new publication is currently underway to describe progresses on this new code. If you use `ALaDyn`, you're kindly required to cite the Zenodo DOI of the latest release: [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.592388.svg)](http://dx.doi.org/10.5281/zenodo.592388).

[Papers published by the ALaDyn Collaboration](https://aladyn.github.io/Papers/)  
[Code description](./docs/pages/DESCRIPTION.md)  
[Input guide](./docs/pages/NAMELIST_GUIDE.md)

Copyright on the code is by the ALaDyn Collaboration.

## How to build

`ALaDyn` is built using CMake.

We support building ALaDyn on almost all sane systems. [Here](./docs/pages/BUILD.md) you can find a guide to build the code on many different OS configurations.  
appveyor and travis recipes can also help to understand how to build the code.

## How to contribute to the development

[Here](./docs/pages/CONTRIBUTING.md) you can find the instructions to propose your changes to `ALaDyn`

## Support channels

We have a Telegram channel to promote latest news: [join here](https://t.me/ALaDyn_Collaboration)  
We also have a group on Telegram ([join here](https://t.me/ALaDyn_Chat)) (for quick questions and unofficial discussions), an [official slack channel](http://aladyn.slack.com) (for more technical discussions) and another slack channel for the [italian plasma community](http://plasmaitaly.slack.com).

## Releases

latest release:  [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.3607652.svg)](http://dx.doi.org/10.5281/zenodo.3607652)

Other releases:  
v2.3.0 (v2019.1): [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2626679.svg)](https://doi.org/10.5281/zenodo.2626679)  
v2.2.0 (v2018.3): [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1477315.svg)](https://doi.org/10.5281/zenodo.1477315)  
v2.1.0 (v2018.2): [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1406920.svg)](https://doi.org/10.5281/zenodo.1406920)  
v2.0.0 (v2018.1): [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1195760.svg)](https://doi.org/10.5281/zenodo.1195760)  
v1.0.0 (v2017.1): [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1065413.svg)](https://doi.org/10.5281/zenodo.1065413)  
v1.0.0-beta: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.49553.svg)](https://doi.org/10.5281/zenodo.49553)  
v1.0.0-alpha2: [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.48933.svg)](http://dx.doi.org/10.5281/zenodo.48933)  
v1.0.0-alpha: [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.47467.svg)](http://dx.doi.org/10.5281/zenodo.47467)
