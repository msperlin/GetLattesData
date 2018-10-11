### Version 1.2 (2018-10-11)

- Changes to the Lattes website seems to be permanent and stable. The main change is that in order to download  xml zip files, one must mannualy break a captcha. Based on this change, the package no longer works by downloading the files, but acessing it from locally (yes, you must download all files manualy). This update fix [Github issue 10](https://github.com/msperlin/GetLattesData/issues/10) and [Github issue 09](https://github.com/msperlin/GetLattesData/issues/09).


### Version 1.0 (2018-08-13)

Lattes website is back without captcha! The new version of GetLattesData is a bit slower than the old one, but it works.

Minor changes:
- [fixed multiple issn issue in SJR](https://github.com/msperlin/GetLattesData/issues/6)


### Version 1.1 (2018-08-19)

Sadly, lattes website is once again using captcha. This version removes the code needed to build the package, so that it can be hosted in CRAN. 

Once again, I'll keep checking Lattes over time and see whether any solution comes to life.

### Version 1.0 (2018-08-13)

Lattes website is back without captcha! The new version of GetLattesData is a bit slower than the old one, but it works.

Minor changes:
- [fixed multiple issn issue in SJR](https://github.com/msperlin/GetLattesData/issues/6)


### Version 0.9 (2017-11-27)

Lattes website is offline. Online downloading of xml files is no longer possible.

- Added comments at main function, which now returns empty df, and vignettes.

### Version 0.8 (2017-11-12)

- Now with support for conferences and accepted articles
- Added a function for finding information about accepted and published papers

### Version 0.7 (2017-10-31)

- Added support for books and books chapters

### Version 0.6 (2017-10-14)

- Forced UTF-8 encoding in all output
- Users can now download information about academic supervisions (msc, phd, ic)

### Version 0.5 (2017-09-04)

First commit
