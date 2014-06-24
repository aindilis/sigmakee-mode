(
 (http://www.ontologyportal.org/WhatsNew.html)
 (http://www.emacswiki.org/emacs/AutoComplete)
 )

(completed (color coding as I did with the gtksourceview file - one color for comments, one for strings, one for variables, one for the seven logical operators (and, or, not, exists, forall, =>, <=>)))
(completed (paren balancing (easy since Emacs does this already)))
(completed (autocompletion - if you had a way to specify a file format for a list of terms and relations, I could write a function for Sigma to export such a file))

(documentation popups - This is a bit more advanced, and so it would be better to get the rest working first, but it would be nice to have something like what Eclipse does - when you write a function name, a little popup (or message in the Emacs command box) might give you the basic type information (superclass for classes, argument types for relations) and a snippet of the documentation string.  Similarly to above, if you have a simple file format for this, I could write a routine for Sigma to export it.)
(I would love an Ubuntu install for Sigma.  You could start with the existing sigma-2-02.zip file as a basis and then we could update from there.  You could build the latest sigma.war file from source.  You can get E at eprover.org.)
(Once we have that working and I have a current bug in SUO-KIF-TPTP translation fixed we could then work on integrating LEO-II properly so we'd have higher order capability.  Chris Benzmueller and I made a start on that years ago.  With a little help and encouragement I bet we could finish the job.)

(
 cvs -d:pserver:anonymous@sigmakee.cvs.sourceforge.net:/cvsroot/sigmakee login
 cvs -z3 -d:pserver:anonymous@sigmakee.cvs.sourceforge.net:/cvsroot/sigmakee co -P modulename
 )

(setq load-path (cons "/var/lib/myfrdcsa/codebases/minor/sigmakee-mode/data-git/daml-mode" load-path))
(load "damlite")
