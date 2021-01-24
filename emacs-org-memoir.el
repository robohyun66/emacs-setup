;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                  ;;;
;;  An export option to the LaTeX class "ebooks" (based on memoir)  ;;;
;;                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'org-latex-classes '("ebook"

"\\documentclass[11pt, oneside]{memoir}

\\setstocksize{9in}{6in}

\\settrimmedsize{\\stockheight}{\\stockwidth}{*}

\\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin

\\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin

\\checkandfixthelayout

\\usepackage{times}

\\usepackage[british]{babel}

\\usepackage[raggedright]{sidecap}

\\setsecheadstyle{\\normalfont \\raggedright \\textbf}

\\setsubsecheadstyle{\\normalfont \\raggedright \\emph}

\\usepackage[labelformat=empty, font=small]{caption}

\\usepackage{pdfpages}

\\usepackage[unicode=true, bookmarks=true,bookmarksnumbered=false,bookmarksopen=true,bookmarksopenlevel=1, breaklinks=true,pdfborder={0 0 0},backref=false,colorlinks=false,pdfborderstyle={/S/U/W .5}, allbordercolors={.8 .8 .8}]{hyperref}

\\pagestyle{myheadings}

\\setcounter{tocdepth}{0}

\\usepackage{ccicons}

\\OnehalfSpacing

\\usepackage[authoryear]{natbib}

"

("\\chapter{%s}" . "\\chapter*{%s}")

("\\section{%s}" . "\\section*{%s}")

("\\subsection{%s}" . "\\subsection*{%s}")

))
