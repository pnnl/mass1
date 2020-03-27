# Cross-section Management Tools

This directory contains several scripts for the manipulation of
*general* cross section data in the format used by CHARIMA (with some
internal comments). This format is very similar to that used by MASS1.
Contrary to the script names, the output format won't work with CHARIMA
anymore, if CHARIMA is actually used anymore.  

These utilities are quite old and provided as-is. There is little or no
documentation (except the code itself).  Some are still used often by the
authors; others are quite old and in probably in need of maintenance.  

## Requirements


* [Perl](https://www.perl.org)

  Standard Perl 5 can be used for all the Perl scripts, except one.

* [Perl Data Language (PDL)](http://pdl.perl.org) with
  [PGPLOT](http://search.cpan.org/~kgb/PGPLOT-2.18/PGPLOT.pm)
  
  Specifically, the`PDL::Graphics::PGPLOT::Window` package is required
  for `CHARIMAview.pl`.

* [Awk](https://en.wikipedia.org/wiki/AWK) or
  (gawk)[https://www.gnu.org/software/gawk] 
  
  The scripts to convert between MASS1 and CHARIMA format are in
  [Awk](https://en.wikipedia.org/wiki/AWK). 

## Cross Section Format

These scripts manipulate cross sections in one or more files.  Many
cross sections can be contained in a file.  

These scripts understand the cross section format used by the CHARIMA
code.  MASS1's format is slightly different.  Only the general cross
section type is understood (type 50). MASS1's prismatic section types
are not handled.

To convert from MASS1 to CHARIMA format:

```
awk -f section-unfix.awk < mass1_section.txt > charima_section.txt
```

To convert from CHARIMA to MASS1 format:

```
awk -f section-fix.awk < charima_section.txt > mass1_section.txt
```

## Utilities

The following are [Perl](https://www.perl.org) script that require the
`XSection.pm` module file. 

* `CHARIMAarea.pl`: computes the cross section area for a set of cross
  sections at a single water surface elevation

    CHARIMAarea.pl [-f] [-e elev] [-o out] file [file ...]

* `CHARIMAextract.pl`: extracts a subset, by either section ID or
  river mile

    CHARIMAextract.pl -I -l id,... [file]
    CHARIMAextract.pl -I -m min_id -M max_id [file]
    CHARIMAextract.pl -R -m min_rm -M max_rm [file]
    CHARIMAextract.pl -R -l id,... [file] at CHARIMAextract.pl line 63.

* `CHARIMAfill.pl`: 

    CHARIMAfill.pl [-o output] base_file fill_file mingap
    
* `CHARIMAinsert.pl`:

    CHARIMAinsert.pl [-o output] base_file insert_file

* `CHARIMAmetric.pl`:

    CHARIMAmetric.pl file [file ...]

* `CHARIMAnotch.pl`: adds a triangular notch at the cross-section(s) thalweg 

    CHARIMAnotch.pl [-v] [-w width] [-h height] [-o output] file [file ...]
    
* `CHARIMArenum.pl`

    CHARIMArenum.pl [-1 num] [-s step] [-o out] file [file...]

* `CHARIMAthalweg.pl`: find the cross-section thalweg (a good step to
  prepare a  [../../doc/point.md](MASS1 point file))
  
    CHARIMAthalweg.pl file [file ...]
    
* `CHARIMAview.pl`: interactive cross-section viewer/plotter

    CHARIMAview.pl [-m] [-d device] file
    
* `HEC2area.pl`: like `CHARIMAarea.pl` but for HEC-2 format cross sections
* `HEC2CHARIMA.pl`: reformat HEC-2 sections to CHARIMA
* `HECRASCHARIMA.pl`: reformat
  [https://www.hec.usace.army.mil/software/hec-ras/](HEC-RAS) sections
  to CHARIMA
* `XSECTprops.pl`: compute 
