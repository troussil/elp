\def \fileversion{v1.0}
\def \filedate{2010/07/05}
\def \docdate{\filedate}
\newif \if@@draft
\NeedsTeXFormat{LaTeX2e}
\usepackage{ifthen}
\usepackage{ragged2e}
\newcommand{\reftexRun}{}\newcommand{\href}[2]{#2}
\newcommand{\setLanguage}{
\def \@authorand{et}
}
\DeclareOption{english}{\renewcommand{\setLanguage}{
\def \@authorand{and}
}}
\ProcessOptions
%
\LoadClass{article}
\RequirePackage{latexsym}
\reftexRun
\setLanguage
\usepackage[margin=3cm]{geometry}
\usepackage{fancyhdr}
\usepackage{xspace}
\usepackage{xcolor}
\pagestyle{fancy}

\def \title{\@ifnextchar[{\@gettitletwo}{\@gettitleone}}
\def \@gettitleone#1{\@gettitletwo[#1]{#1}}
\def \@gettitletwo[#1]#2{{%
    \gdef \@title{#2}%
    \def \thanks##1{\relax}%
    \def \\{\unskip \space \ignorespaces}%
    \xdef \@titlehead{#1}%
}}

\newif \if@marksrequired
\def \author{\@ifnextchar[{\@getauthortwo}{\@getauthorone}}
\def \@getauthorone#1{\@getauthortwo[#1]{#1}}
\def \@getauthortwo[#1]#2{{%
    \global \@marksrequiredtrue%
    \global \let \latexand \and%
    \gdef \and{\unskip\ignorespaces\\}%
    \gdef \@author{\if@@draft \strut \else #2\fi}%
    %\def \thanks##1{\relax}%
    %\def \\{\unskip \space \ignorespaces}%
    \@marksrequiredfalse%
	%\gdef \and{\unskip \space \@authorand \space \ignorespaces}%
    \gdef \@authorhead{\if@@draft \space \else #1\fi}% was xdef
}}

\renewcommand \maketitle{\par%
        %\def \@makefnmark{\hb@xt@ \z@{$\m@th^{\@thefnmark}$\hss}}%
        %\long \def \@makefntext##1{\parindent 1em\noindent
        %    \hb@xt@ 1.8em{\hss $\m@th^{\@thefnmark}$}##1}%
        \global \@topnum \z@%
		\begin{minipage}{\textwidth}
		\raggedright \includegraphics[width=4cm]{logo.pdf}
		\end{minipage}\vspace{-1.1cm}
		\begin{minipage}{\textwidth}
		\raggedleft \Large Département Télécommunications,\\ Services et Usages
		\end{minipage}
		{\centering
        \@maketitle%
		\vfill
		\begin{minipage}{\textwidth}
		%\centering \includegraphics[width=\textwidth]{pics/geek}
		\end{minipage}
		\vfill}
		{\raggedright
		\large
		Édition \today }
        \thispagestyle{empty}%
    \global \let \maketitlee \relax%
    \global \let \@maketitlee \relax%
    \gdef \@author{}%
%    \gdef \@thanks{}%
    \gdef \@title{}%
	\clearpage
%    \global \let \thanks \relax%
}


\def \@maketitle{
%\vspace{\fill}
~\\
\vspace{190\p@}
{\noindent \centering \fbox{
\begin{minipage}{\textwidth}\centering\vspace{0.05\textwidth}
\begin{minipage}{0.9\textwidth}
  \centering{\Huge \centering \@title}
\end{minipage}\vspace{0.05\textwidth}
\end{minipage}
  } \vspace{0.1\textwidth}
  
\centering{\Huge \centering \@author}\vspace{0.1\textwidth}


\par}
}  %fin du titre


\flushbottom

\endinput
%% 
%% End of file `dmtcs.cls'.
