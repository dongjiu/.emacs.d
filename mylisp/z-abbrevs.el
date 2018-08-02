;; shell
(if (eq system-type 'windows-nt)
	(define-skeleton z-shell-find
	  "Run Linux find command."
	  nil
	  '(setq v1 default-directory)
	  "async-shell-command 'es " v1 " " _ "'")
  (define-skeleton z-shell-find
	"Run Linux find command."
	nil
	"find $PWD -iname '" _ "'"))

(define-skeleton z-shell-find-text
  "Run perl find_text."
  nil
  '(setq v1 default-directory)
  "async-shell-command 'es " v1 " " _ " | perl " (expand-file-name "~/.emacs.d/perl/filter_files.pl") " '")

(define-skeleton z-shell-git-commit
  "git commit."
  nil
  "git commit -m'" _ "'")

(define-skeleton z-shell-git-merge
  "git merge."
  nil
  "git merge " _ " -m'merge '")

(define-skeleton z-shell-perl-one-liner
  "perl one liner."
  nil
  "perl -E 'say " _ "'")

(define-skeleton z-shell-perl-path
  "perl one liner to print path."
  nil
  "perl -e 'print \"$_\\n\" for split /"
  path-separator
  "/, $ENV{PATH}'")

(define-skeleton z-shell-perl-line-count
  "count lines using perl."
  nil
  "perl -E '++$cnt while <>;say $cnt;'")

(define-abbrev-table 'z-shell-mode-abbrev-table
  '(
	("fn" "" z-shell-find)
	("ft" "" z-shell-find-text)
	("lc" "" z-shell-perl-line-count)
	("zs" "z-run-everything-search-in-current-dir")

	("gs" "git status")
	("gb" "git branch")
	("gba" "git branch -a")
	("go" "git checkout")
	("gpu" "git push origin")
	("gr" "git revert --no-commit")
	("ga" "git add")
	("gp" "git pull")
	("gpom" "git pull origin master")
	("gst" "git stash")
	("gsp" "git stash pop")
	("gc" "" z-shell-git-commit)
	("gls" "git ls-files")
	("gm" "" z-shell-git-merge)

	("plp" "" z-shell-perl-one-liner)
	("plpath" "" z-shell-perl-path)
	("scts" "shell-command-to-string")
	))

(push (cons 'z-shell-mode z-shell-mode-abbrev-table)
	  abbrev-minor-mode-table-alist)

(define-abbrev-table 'eshell-mode-abbrev-table
  '(
	("cls" "z-eshell-clear-buffer")
	))

;; sql
(define-skeleton z-sql-mysql-proc
  "Inserts mysql procedure."
  "Procedure name: "
  "DELIMITER $$\n"
  "CREATE PROCEDURE " str "(" _ ")\n"
  "BEGIN\n\n"
  "END$$\n"
  "DELIMITER ;\n")

(define-skeleton z-sql-exists
  "Insert exists block."
  nil
  "exists (" _ ")")

(define-skeleton z-sql-in
  "Insert in block."
  nil
  "in (" _ ")")

(define-abbrev-table 'sql-mode-abbrev-table
  '(
	("cm" "/* */" (lambda () (backward-char 3)))
	("lm" "limit 20")
	("zs" "select * from")
	("zsc" "select count(*) from")
	("zw" "where")
	("zex" "" z-sql-exists)
	("zin" "" z-sql-in)
	))

;; perl
(define-skeleton z-perl-shebang
  "Inserts perl shebang line and common use lines"
  nil
  "#!/usr/bin/env perl\n"
  "use v5.14;\n"
  )

(define-skeleton z-perl-here-doc
  "Inserts perl here document"
  nil
  "<<EOF\n"
  _
  "\nEOF\n;\n"
  )

(defun z-perl-sub ()
  "Inserts perl sub"
  (interactive)
  (insert "sub ")
  (insert (read-string "Sub name: "))
  (insert " {\n")
  (let ((stop nil) (arg) (narg 0))
	(while (not stop)
	  (setq arg (read-string "Arg: "))
	  (if (not (string= arg ""))
		  (progn (setq narg (1+ narg))
				 (if (= narg 1)
					 (progn (cperl-indent-command)
							(insert "my ($")
							(insert arg))
				   (insert ", $" arg)))
		(progn (setq stop t)
			   (when (> narg 0) (insert ") = @_;"))
			   (insert "\n")
			   ))))
  (insert "\n}")
  (backward-char 2)
  )

(define-abbrev-table 'cperl-mode-abbrev-table
  '(
	("sb" "" z-perl-shebang)
	("sub" "" z-perl-sub)
	("dp" "print Dumper")
	("hdoc" "" z-perl-here-doc)
	))

;; python
(define-abbrev-table 'python-mode-abbrev-table
  '(
	("zplt" "import matplotlib.pyplot as plt")
	("znp" "import numpy as np")
	))


;; c family
(define-skeleton z-code-c-stopif
  "Insert Stopif macro"
  nil
  "#define Stopif(assertion, error_action, ...) {\\"
\n "if (assertion) {\\"
\n "fprintf(stderr, __VA_ARGS__);\\"
\n	"fprintf(stderr, \"\\n\");\\"
\n "{ error_action; }\\"
\n "}}")

(defun z-code-block ()
  "Insert empty code block"
  (insert "{")
  (c-indent-line-or-region)
  (newline)
  (c-indent-line-or-region)
  (newline)
  (insert "}")
  (c-indent-line-or-region))

(defun z-code-block-interactive ()
  "Insert empty code block with curly braces"
  (interactive)
  (z-code-block)
  (previous-line)
  (end-of-line)
  t)
(put 'z-code-block-interactive 'no-self-insert t)

(define-skeleton z-code-if-block
  "Insert if block"
  nil
  "if (" _ ") "
  '(z-code-block))

(define-skeleton z-code-if-block-long
  "Insert if block with curly braces on separate lines"
  nil
  "if (" _ ")\n"
  '(z-code-block))

(define-skeleton z-code-if
  "Insert if block"
  nil
  "if (" _ ")")

(define-skeleton z-code-else
  "Insert else block"
  nil
  "else "
  '(z-code-block))

(define-skeleton z-code-else-long
  "Insert else block with curly braces on separate lines"
  nil
  '(c-indent-line-or-region)
  "else\n{"
  '(c-indent-line-or-region)
  "\n\n"
  "}"
  '(c-indent-line-or-region)
  '(previous-line)
  '(c-indent-line-or-region))

(define-skeleton z-code-for-block
  "Insert for loop"
  nil
  "for (" _ ") "
  '(z-code-block))

(define-skeleton z-code-for
  "Insert for loop"
  nil
  "for (" _ ")")

(define-skeleton z-code-for-long
  "Insert for loop with curly braces on separate lines"
  nil
  "for (" _ ")\n"
  '(z-code-block))

(define-skeleton z-code-while-block
  "Insert while loop"
  nil
  "while (" _ ") "
  '(z-code-block))

(define-skeleton z-code-switch-block-long
  "Insert switch loop"
  nil
  "switch (" _ ")\n"
  '(z-code-block))

(define-skeleton z-code-while
  "Insert while loop"
  nil
  "while (" _ ")")

(define-skeleton z-code-while-block-long
  "Insert while loop with curly braces on separate lines"
  nil
  "while (" _ ")\n"
  '(z-code-block))

(define-abbrev-table 'z-code-mode-abbrev-table
  '(
	("zb" "" z-code-block-interactive)
	("ifb" "" z-code-if-block)
	("zif" "" z-code-if-block-long)
	("if" "" z-code-if)
	("el" "" z-code-else)
	("zel" "" z-code-else-long)
	("whb" "" z-code-while-block)
	("zwh" "" z-code-while-block-long)
	("wh" "" z-code-while)
	("fo" "" z-code-for-block)
	("for" "" z-code-for)
	("zfo" "" z-code-for-long)
	("zsw" "" z-code-switch-block-long)
	))

(push (cons 'z-code-mode z-code-mode-abbrev-table)
	  abbrev-minor-mode-table-alist)

;; c
(define-skeleton z-c-single-file
  "Insert single file C template"
  nil
  "#include <stdio.h>\n"
  "#include <stdlib.h>\n"
  "#include <string.h>\n"
  "#include <limits.h>\n\n"
  "int main() {\n"
  _
  "\n}\n")

(define-skeleton z-c-malloc
  "Insert malloc."
  "Type: "
  str " *"
  '(setq v1 (read-string "Variable: "))
  v1 " = (" str " *) malloc(sizeof(" str ")"
  '(setq v2 (read-string "Size (1): "))
  '(if (string= v2 "")
	   (insert ");")
	 (insert " * " v2 ");"))
  )

(define-skeleton z-c-main
  "Insert main function without args."
  nil
  "int\nmain(void)\n{\n"
  _
  "\n	exit(0);\n}")

(define-skeleton z-c-main-with-args
  "Insert main function with args."
  nil
  "int\nmain(int argc, char *argv[])\n{\n"
  _
  "\n	exit(0);\n}")

(define-abbrev-table 'c-mode-abbrev-table
  '(
	("sf" "" z-c-single-file)
	("cmain" z-c-main)
	("cmaina" z-c-main-with-args)
	("ma" "" z-c-malloc)
	))

;; c++
(define-skeleton z-cpp-single-file
  "Insert single file C++ template"
  nil
  "#include <iostream>\n"
  "#include <string>\n"
  "#include <vector>\n"
  "#include <list>\n"
  "#include <stack>\n"
  "#include <map>\n"
  "#include <set>\n"
  "#include <algorithm>\n\n"
  "using namespace std;\n\n"
  "int main() {\n"
  _
  "\n}\n")

(define-abbrev-table 'c++-mode-abbrev-table
  '(
	("sf" "" z-cpp-single-file)
	("v" "vector<> " (lambda () (backward-char 2)))
	))

;; java
(defun z-java-class ()
  "Insert java class"
  (interactive)
  (let ((pkg) (class) (base) (interface))
	(setq pkg (read-string "Package name: "))
	(when (not (string= pkg "")) (insert "package " pkg ";\n\n"))
	(insert "import java.util.*;\n\n")
	(setq class (file-name-sans-extension (buffer-name)))
	(insert "public class " class)
	(setq base (read-string "Base class: "))
	(when (not (string= base "")) (insert " extends " base))
	(setq interface (read-string "Interface: "))
	(when (not (string= interface "")) (insert " implements " interface))
	(insert " {\n")
	(c-indent-line)
	(insert "public " class "() {\n\n")
	(insert "    }\n")
	(insert "}\n")
	(previous-line 3)
  ))
  
(define-skeleton z-java-bean
  "Insert java bean"
  nil
  '(setq v1 (read-string "Type (String): "))
  '(when (string= v1 "") (setq v1 "String"))
  '(setq v2 (read-string "Variable name: "))
  "private " v1 " " v2 ";"
  \n "public " v1 " get" (upcase (substring v2 0 1)) (substring v2 1) "() { return " v2 "; }"
  \n "public void set" (upcase (substring v2 0 1)) (substring v2 1) "(" v1 " " v2 ") { this." v2 " = " v2 "; }")

(define-skeleton z-java-newlist
  "New List"
  nil
  '(setq str (read-string "Element type (String): "))
  '(when (string= str "") (setq str "String"))
  "List<" str "> " _ " = new ArrayList<" str ">();"
  )

(define-skeleton z-java-newmap
  "New Map"
  nil
  '(setq str (read-string "Key type (String): "))
  '(when (string= str "") (setq str "String"))
  '(setq v1 (read-string "Value type (String): "))
  '(when (string= v1 "") (setq v1 "String"))
  "Map<" str ", " v1 "> " _ " = new HashMap<" str ", " v1 ">();"
  )

(define-skeleton z-java-newset
  "New Set"
  nil
  '(setq str (read-string "Element type (String): "))
  '(when (string= str "") (setq str "String"))
  "Set<" str "> " _ " = new HashSet<" str ">();"
  )

(define-skeleton z-java-comment
  "New comment."
  nil
  "/**\n*"
  '(c-indent-line-or-region) " " _ "\n*"
  '(c-indent-line-or-region) "/"
  )

(define-skeleton z-java-servlet
  "New servlet."
  "Package: "
  "package " str ";\n\n"
  "import java.io.IOException;\n"
  "import javax.servlet.*;\n"
  "import javax.servlet.http.*;\n\n"
  "public class " (file-name-sans-extension (buffer-name)) " extends HttpServlet {\n"
  > "public void init(ServletConfig conf) throws ServletException {\n"
  > > "super.init(conf);\n"
  > -4 "}\n\n"
  > "public void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {\n"
  > _ "\n"
  > -4 "}\n\n"
  > "public void doPost(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {\n"
  > -4 "}\n"
  "}")

(define-skeleton z-java-servlet-forward
  "Forward to url"
  nil
  "getServletContext().getRequestDispatcher(" _ ").forward(req, res);")

(define-abbrev-table 'java-mode-abbrev-table
  '(
	("cl" "" z-java-class)
	("bn" "" z-java-bean)
	("nlist" "" z-java-newlist)
	("nmap" "" z-java-newmap)
	("nset" "" z-java-newset)
	("zcm" "" z-java-comment)
	("sf" "String.format(\"\", )" (lambda () (backward-char 4)))
	("pl" "System.out.println();" (lambda () (backward-char 2)))
	("nserv" "" z-java-servlet)
	("fwd" "getServletContext().getRequestDispatcher().forward(req, res);" (lambda () (backward-char 20)))
	))

(define-skeleton z-java-config-servlet
  "Insert servlet configuration."
  "Servlet Name: "
  "<servlet>\n"
  > "<servlet-name>" str "</servlet-name>\n"
  > "<servlet-class>" _ "</servlet-class>\n"
  "</servlet>\n\n"
  "<servlet-mapping>\n"
  > "<servlet-name>" str "</servlet-name>\n"
  > "<url-pattern></url-pattern>\n"
  "</servlet-mapping>\n")

(define-skeleton z-java-config-init-param
  "Insert init-param element."
  nil
  "<init-param>\n"
  > "<param-name>" _ "</param-name>\n"
  > "<param-value></param-value>\n"
  "</init-param>\n")

(define-skeleton z-java-maven-dep-servlet
  "Insert servlet dependency."
  nil
  "<dependency>\n"
  > "<groupId>javax.servlet</groupId>\n"
  > "<artifactId>javax.servlet-api</artifactId>\n"
  > "<version>3.0.1</version>\n"
  > "<scope>provided</scope>\n"
  "</dependency>\n")

;; make file
(define-skeleton z-make-gcc-single-file
  "gcc single file executable"
  "Target: "
  str ": " str ".c\n"
  "\tgcc -g " str ".c -o " str)

(define-skeleton z-make-clang++-single-file
  "clang++ single file executable"
  "Target: "
  str ": " str ".cpp\n"
  "\tclang++ -g -std=c++98 " str ".cpp -o " str)

(define-abbrev-table 'makefile-bsdmake-mode-abbrev-table
  '(
	("ce" "" z-make-gcc-single-file)
	("cppe" "" z-make-clang++-single-file)
	))

;; web
(define-skeleton z-web-html
  "Insert html template"
  "Title: "
  "<!DOCTYPE html>\n"
  "<html>\n"
  "<head>\n"
  >  "<title>" str "</title>\n"
  "</head>\n"
  "<body>\n"
  _
  "\n</body>\n"
  "</html>"
  )

(define-skeleton z-web-tag
  "Insert html tag."
  "Tag name:"
  "<" str ">" _ "</" str ">")

(define-skeleton z-web-freemarker-macro
  "Insert freemarker macro."
  nil
  "<#macro " _ ">\n</#macro>")

(define-skeleton z-web-freemarker-list
  "Insert freemarker list expression."
  nil
  "<#list " _ "></#list>")

(define-skeleton z-web-svg-file
  "Insert svg file."
  nil
  "<?xml version=\"1.0\"?>\n"
  "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.0//EN\"\n"
  "\"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd\">\n"
  "<svg>\n"
  _
  "\n</svg>\n")

(define-skeleton z-web-js-function
  "Insert JavaScript function."
  nil
  "function () {\n"
  '(indent-for-tab-command) _
  "\n}"
  '(indent-for-tab-command)
  "\n")

(define-skeleton z-svg-circle
  "Insert circle element."
  nil
  "<circle cx=\"" _ "\" cy=\"\" r=\"\" style=\"stroke: black; fill: none\" />")

(define-skeleton z-svg-line
  "Insert line element."
  nil
  "<line x1=\"" _ "\" y1=\"\" x2=\"\" y2=\"\" style=\"stroke: black;\" />")

(define-skeleton z-svg-polyline
  "Insert polyline element."
  nil
  "<polyline points=\"" _ "\" style=\"stroke: black;\" />")

(define-skeleton z-svg-polygon
  "Insert polygon element."
  nil
  "<polygon points=\"" _ "\" style=\"stroke: black; fill: none;\" />")

(define-skeleton z-svg-rect
  "Insert rect element."
  nil
  "<rect x=\"" _ "\" y=\"\" width=\"\" height=\"\" style=\"stroke: black; fill: none;\" />")

(define-skeleton z-svg-path
  "Insert path element."
  nil
  "<path d=\"M " _ "\" style=\"stroke: black; fill: none\" />")

(define-skeleton z-svg-text
  "Insert text element."
  nil
  "<text x=\"" _ "\" y=\"\" style=\"font-family: sans-serif; font-size: 14pt; stroke: black; fill: black;\"></text>")

(define-skeleton z-svg-ellipse
  "Insert ellipse element."
  nil
  "<ellipse cx=\"" _ "\" cy=\"\" rx=\"\" ry=\"\" style=\"stroke: black; fill: none;\" />")

(define-abbrev-table 'z-web-mode-abbrev-table
  '(
	("zhtml" "" z-web-html)
	("za" "<a href=\"\"></a>" (lambda () (backward-char 6)))
	("zt" "" z-web-tag)
	("zinp" "<input type=\"\" value=\"\" />" (lambda () (backward-char 13)))
	("zcss" "<link rel=\"stylesheet\" type=\"text/css\" href=\"\" />" (lambda () (backward-char 4)))

	("zjs" "<script></script>" (lambda () (backward-char 9)))
	("zjsref" "<script src=\"\"></script>" (lambda () (backward-char 11)))
	("zf" "" z-web-js-function)
	("zdrd" "$(document).ready(function () {\n});" (lambda () (backward-char 4)))
	("zfo" "for () {\n}" (lambda () (backward-char 5)))
	("zif" "if () {\n}" (lambda () (backward-char 5)))

	("zfm" "" z-web-freemarker-macro)
	("zfl" "" z-web-freemarker-list)
	("zfi" "<#import \"\">" (lambda () (backward-char 2)))

	("zsvgf" "" z-web-svg-file)
	))

(push (cons 'z-web-mode z-web-mode-abbrev-table)
	  abbrev-minor-mode-table-alist)

;; tex-mode
(define-skeleton z-latex-article
  "Insert latex article template"
  nil
  "\\documentclass{article}\n"
  "\\begin{document}\n"
  _
  "\n\\end{document}")

(define-skeleton z-latex-standalone
  "Insert latex article template"
  nil
  "\\documentclass{standalone}\n"
  "\\begin{document}\n"
  _
  "\n\\end{document}")

(define-skeleton z-latex-environment
  "Insert latex environment"
  "Environment: "
  "\\begin{" str "}\n"
  _
  "\n\\end{" str "}")

(define-abbrev-table 'latex-mode-abbrev-table
  '(
	("tart" "" z-latex-article)
	("tsa" "" z-latex-standalone)
	("tenv" "" z-latex-environment)
	("tpkg" "\\usepackage{}" (lambda () (backward-char 1)))
	))

;; C#
(define-skeleton z-csharp-foreach
  "Insert foreach loop."
  nil
  "foreach (" _ ")\n"
  '(z-code-block))

(define-skeleton z-csharp-newlist
  "New List."
  "Type: "
  "var " _ " = new List<" str ">();")

(define-skeleton z-csharp-newset
  "New HashSet."
  "Type: "
  "var " _ " = new HashSet<" str ">();")

(define-skeleton z-csharp-newdict
  "New Dictionary."
  "Key Type, Value Type: "
  "var " _ " = new Dictionary<" str ">();")

(define-skeleton z-csharp-namespace
  "Namespace block."
  nil
  "namespace " _ "\n"
  '(z-code-block))

(define-skeleton z-csharp-class
  "New class."
  nil
  "sealed class " _ "\n"
  '(z-code-block))

(define-skeleton z-csharp-main
  "Main function."
  nil
  "static int Main(string[] args)\n"
  '(z-code-block)
  '(previous-line)
  '(end-of-line))

(define-skeleton z-csharp-write-line
  "Insert Console.WriteLine()."
  nil
  "Console.WriteLine(" _ ");")

(defun z-csharp-property (&optional readonly)
  "Insert property."
  (let ((type) (back-field-name))
	(setq type (read-string "Back field type? "))
	(setq back-field-name (read-string "Back field name? "))
	(c-indent-line-or-region)
	(insert (format "public %s %s\n"
					type
					(z-string-upcase-first-char back-field-name)))
	(c-indent-line-or-region)
	(insert "{\n")
	(c-indent-line-or-region)
	(insert (format "get { return %s; }\n" back-field-name))
	(unless readonly
	  (c-indent-line-or-region)
	  (insert (format "set { this.%s = value; }\n" back-field-name)))
	(insert "}")
	(c-indent-line-or-region)
	(insert "\n")
	)
  t)
(put 'z-csharp-property 'no-self-insert t)

(defun z-csharp-readonly-property ()
  "Insert readonly property."
  (z-csharp-property t)
  t)
(put 'z-csharp-readonly-property 'no-self-insert t)

(define-abbrev-table 'csharp-mode-abbrev-table
  '(
	("zfe" "" z-csharp-foreach)
	("zlist" "" z-csharp-newlist)
	("zset" "" z-csharp-newset)
	("zdict" "" z-csharp-newdict)
	("zwl" "" z-csharp-write-line)

	("zusys" "using System;")
	("zuio" "using System.IO;")
	("zugen" "using System.Collections.Generic;")

	("zns" "" z-csharp-namespace)
	("zcl" "" z-csharp-class)
	("zmain" "" z-csharp-main)
	("zp" "" z-csharp-readonly-property)
	("zpr" "" z-csharp-property)
	))

;; msbuild
(defun z-xml-head ()
  "Insert xml head line."
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"))

(define-skeleton z-msbuild-proj
  "Insert root Project element."
  nil
  '(z-xml-head)
  "<Project xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">\n"
  '(indent-for-tab-command)
  _
  "\n</Project>\n")

(define-skeleton z-msbuild-target
  "Insert Target element."
  nil
  "<Target Name=\"" _ "\">\n"
  '(indent-for-tab-command)
  \n
  '(indent-for-tab-command)
  "</Target>"
  '(indent-for-tab-command))

(define-skeleton z-msbuild-msg
  "Insert Message element."
  nil
  "<Message Text=\"" _ "\"></Message>")

(defun z-xml-elem (name)
  "Insert xml element."
  (insert (concat "<" name "></" name ">"))
  (backward-char (+ (length name) 3)))

(defun z-xml-elem-long (name)
  "Insert xml element in two lines."
  (z-xml-elem name)
  (newline)
  (indent-for-tab-command)
  (previous-line)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun z-msbuild-elem ()
  "Insert an msbuild element."
  (let ((name))
	(setq name (read-string "Element name: "))
	(z-xml-elem name))
  t)
(put 'z-msbuild-elem 'no-self-insert t)

(defun z-msbuild-property-group ()
  "Insert PropertyGroup element."
  (z-xml-elem-long "PropertyGroup")
  t)
(put 'z-msbuild-property-group 'no-self-insert t)

(defun z-msbuild-item-group ()
  "Insert ItemGroup element."
  (z-xml-elem-long "ItemGroup")
  t)
(put 'z-msbuild-item-group 'no-self-insert t)

(define-skeleton z-msbuild-csc
  "Insert Csc element."
  nil
  "<Csc Sources=\"@(" _ ")\"></Csc>")

(define-skeleton z-msbuild-exec
  "Insert Exec element."
  nil
  "<Exec Command=\"" _ "\"></Exec>")

(define-skeleton z-msbuild-reference
  "Insert Reference element."
  nil
  "<Reference Include=\"" _ "\"></Reference>")

(define-skeleton z-msbuild-removedir
  "Insert RemoveDir element."
  nil
  "<RemoveDir Directories=\"" _ "\"></RemoveDir>")

(define-skeleton z-msbuild-mkdir
  "Insert MakeDir element."
  nil
  "<MakeDir Directories=\"" _ "\"></MakeDir>")

(define-skeleton z-msbuild-copy
  "Insert Copy element."
  nil
  "<Copy SourceFiles=\"@(" _ ")\" DestinationFolder=\"\"></Copy>")

(define-abbrev-table 'z-msbuild-mode-abbrev-table
  '(
	("zp" "" z-msbuild-proj)
	("zt" "" z-msbuild-target)
	("ztdep" "DependsOnTargets")
	("zm" "" z-msbuild-msg)
	("ze" "" z-msbuild-elem)
	("zpg" "" z-msbuild-property-group)
	("zig" "" z-msbuild-item-group)
	("zcsc" "" z-msbuild-csc)
	("zexec" "" z-msbuild-exec)
	("zref" "" z-msbuild-reference)
	("zrmd" "" z-msbuild-removedir)
	("zmkd" "" z-msbuild-mkdir)
	("zcp" "" z-msbuild-copy)
	))

(push (cons 'z-msbuild-mode z-msbuild-mode-abbrev-table)
	  abbrev-minor-mode-table-alist)

(define-skeleton z-javascript-log
  "Add console.log."
  nil
  "console.log(" _ ");")

(define-skeleton z-typescript-interface
  "Add interface."
  nil
  "interface " _ " "
  '(z-code-block))

(define-skeleton z-typescript-class
  "Add class."
  nil
  "class " _ " "
  '(z-code-block))

(define-skeleton z-typescript-constructor
  "Add constructor."
  nil
  "constructor(" _ ") "
  '(z-code-block))

(define-abbrev-table 'typescript-mode-abbrev-table
  '(
	("zin" "" z-typescript-interface)
	("zcl" "" z-typescript-class)
	("zct" "" z-typescript-constructor)
	("zlg" "" z-javascript-log)
	))


(define-skeleton z-org-insert-el-code
  "Insert emacs lisp code."
  nil
  "#+BEGIN_SRC emacs-lisp\n"
  _
  "\n#+END_SRC\n")

(define-skeleton z-org-insert-perl-code
  "Insert perl code."
  nil
  "#+BEGIN_SRC perl\n"
  _
  "\n#+END_SRC\n")

(define-skeleton z-org-insert-python-code
  "Insert python code."
  nil
  "#+BEGIN_SRC python\n"
  _
  "\n#+END_SRC\n")

(define-skeleton z-org-insert-sql-code
  "Insert SQL code."
  nil
  "#+BEGIN_SRC sql\n"
  _
  "\n#+END_SRC\n")

(define-abbrev-table 'org-mode-abbrev-table
  '(
	("zel" "" z-org-insert-el-code)
	("zpl" "" z-org-insert-perl-code)
	("zpy" "" z-org-insert-python-code)
	("zsql" "" z-org-insert-sql-code)
	))
