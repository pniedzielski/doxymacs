## Process this file with automake to produce Makefile.in

# When creating the tarball, create .el files with default values
# substituted in the .el.in files for people who don't want to run
# autoconf.

EXTRA_DIST=doxymacs.el doxymacs-xml-parse.el
CONFIG_CLEAN_FILES=doxymacs.el doxymacs-xml-parse.el

doxymacs.el: ${top_srcdir}/lisp/doxymacs.el.in ${top_srcdir}/configure.ac
	sed -e 's/\@VERSION\@/${VERSION}/g ; s/\@DOXYMACS_DEFAULT_STYLE\@/${DOXYMACS_DEFAULT_STYLE}/g ; s/\@DOXYMACS_USE_EXTERNAL_XML_PARSER\@/nil/g ; s/\@DOXYMACS_PARSER\@//g' < $< > $@

doxymacs-xml-parse.el: ${top_srcdir}/lisp/doxymacs-xml-parse.el
	cp $< $@
