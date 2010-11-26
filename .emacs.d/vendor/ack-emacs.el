<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
<link rel="icon" href="/addons/static/hgicon.png" type="image/png">
<meta name="robots" content="index, nofollow" />
<link rel="stylesheet" href="/addons/static/style.css" type="text/css" />

<title>EmacsCustomisations:ack-emacs.el</title>
</head>
<body>

<div class="buttons">
<a href="/addons/log/66">changelog</a>
<a href="/addons/shortlog/66">shortlog</a>
<a href="/addons/tags">tags</a>
<a href="/addons/rev/5b737eefe5ea">changeset</a>
<a href="/addons/file/5b737eefe5ea/">files</a>
<a href="/addons/log/5b737eefe5ea/ack-emacs.el">revisions</a>
<a href="/addons/annotate/5b737eefe5ea/ack-emacs.el">annotate</a>
<a href="/addons/raw-file/5b737eefe5ea/ack-emacs.el">raw</a>
</div>

<h2>ack-emacs.el</h2>

<table>
<tr>
 <td class="metatag">changeset 66:</td>
 <td><a href="/addons/rev/5b737eefe5ea">5b737eefe5ea</a></td></tr>
<tr><td class="metatag">parent:</td><td><a href="/addons/file/1fae210bc469/ack-emacs.el">1fae210bc469</a></td></tr>

<tr>
 <td class="metatag">author:</td>
 <td>&#107;&#105;&#109;&#46;&#118;&#97;&#110;&#119;&#121;&#107;</td></tr>
<tr>
 <td class="metatag">date:</td>
 <td>Wed Nov 10 15:19:03 2010 +0200 (2 weeks ago)</td></tr>
<tr>
 <td class="metatag">permissions:</td>
 <td>-rw-r--r--</td></tr>
<tr>
  <td class="metatag">description:</td>
  <td>Adding CSharp Mode and Google Weather</td>
</tr>
</table>

<pre>
<div class="parity0"><a class="lineno" href="#l1" id="l1">     1</a>(require 'compile)
</div><div class="parity1"><a class="lineno" href="#l2" id="l2">     2</a>(require 'thingatpt)
</div><div class="parity0"><a class="lineno" href="#l3" id="l3">     3</a>
</div><div class="parity1"><a class="lineno" href="#l4" id="l4">     4</a>(defvar ack-command &quot;c:/utils/ack.pl&quot; &quot;The command run by the ack function.&quot;)
</div><div class="parity0"><a class="lineno" href="#l5" id="l5">     5</a>
</div><div class="parity1"><a class="lineno" href="#l6" id="l6">     6</a>(defvar ack-mode-font-lock-keywords
</div><div class="parity0"><a class="lineno" href="#l7" id="l7">     7</a>  '((&quot;^\\(Compilation\\|Ack\\) started.*&quot;
</div><div class="parity1"><a class="lineno" href="#l8" id="l8">     8</a>     (0 '(face nil message nil help-echo nil mouse-face nil) t))))
</div><div class="parity0"><a class="lineno" href="#l9" id="l9">     9</a>
</div><div class="parity1"><a class="lineno" href="#l10" id="l10">    10</a>(defvar ack-use-search-in-buffer-name t
</div><div class="parity0"><a class="lineno" href="#l11" id="l11">    11</a>  &quot;If non-nil, use the search string in the ack buffer's name.&quot;)
</div><div class="parity1"><a class="lineno" href="#l12" id="l12">    12</a>
</div><div class="parity0"><a class="lineno" href="#l13" id="l13">    13</a>(define-compilation-mode ack-mode &quot;Ack&quot;
</div><div class="parity1"><a class="lineno" href="#l14" id="l14">    14</a>  &quot;Specialization of compilation-mode for use with ack.&quot;
</div><div class="parity0"><a class="lineno" href="#l15" id="l15">    15</a>  nil)
</div><div class="parity1"><a class="lineno" href="#l16" id="l16">    16</a>
</div><div class="parity0"><a class="lineno" href="#l17" id="l17">    17</a>(defun ack (dir pattern args)
</div><div class="parity1"><a class="lineno" href="#l18" id="l18">    18</a>  &quot;Run ack, with user-specified ARGS, and collect output in a buffer.
</div><div class="parity0"><a class="lineno" href="#l19" id="l19">    19</a>While ack runs asynchronously, you can use the \\[next-error] command to
</div><div class="parity1"><a class="lineno" href="#l20" id="l20">    20</a>find the text that ack hits refer to. The command actually run is
</div><div class="parity0"><a class="lineno" href="#l21" id="l21">    21</a>defined by the ack-command variable.&quot;
</div><div class="parity1"><a class="lineno" href="#l22" id="l22">    22</a>  (interactive (list (read-file-name &quot;Run ack in directory: &quot; nil &quot;&quot; t)
</div><div class="parity0"><a class="lineno" href="#l23" id="l23">    23</a>                     (read-string &quot;Search for: &quot; (thing-at-point 'symbol))
</div><div class="parity1"><a class="lineno" href="#l24" id="l24">    24</a>                     (read-string &quot;Ack arguments: &quot; nil nil &quot;-i&quot; nil)
</div><div class="parity0"><a class="lineno" href="#l25" id="l25">    25</a>                                  ))
</div><div class="parity1"><a class="lineno" href="#l26" id="l26">    26</a>  ; Get dir into an the right state, incase a file name was used
</div><div class="parity0"><a class="lineno" href="#l27" id="l27">    27</a>    (setq dir (abbreviate-file-name
</div><div class="parity1"><a class="lineno" href="#l28" id="l28">    28</a>               (file-name-as-directory (expand-file-name dir))))
</div><div class="parity0"><a class="lineno" href="#l29" id="l29">    29</a>    ;; Check that it's really a directory.
</div><div class="parity1"><a class="lineno" href="#l30" id="l30">    30</a>    (or (file-directory-p dir)
</div><div class="parity0"><a class="lineno" href="#l31" id="l31">    31</a>        (error &quot;ack needs a directory: %s&quot; dir))
</div><div class="parity1"><a class="lineno" href="#l32" id="l32">    32</a>
</div><div class="parity0"><a class="lineno" href="#l33" id="l33">    33</a>  (let (compile-command
</div><div class="parity1"><a class="lineno" href="#l34" id="l34">    34</a>        (compilation-error-regexp-alist grep-regexp-alist)
</div><div class="parity0"><a class="lineno" href="#l35" id="l35">    35</a>        (compilation-directory default-directory)
</div><div class="parity1"><a class="lineno" href="#l36" id="l36">    36</a>        (ack-full-buffer-name (concat &quot;*ack-&quot; pattern &quot;*&quot;)))
</div><div class="parity0"><a class="lineno" href="#l37" id="l37">    37</a>;;    (save-some-buffers (not compilation-ask-about-save) nil)
</div><div class="parity1"><a class="lineno" href="#l38" id="l38">    38</a>    ;; lambda defined here since compilation-start expects to call a function to get the buffer name
</div><div class="parity0"><a class="lineno" href="#l39" id="l39">    39</a>    (compilation-start (concat ack-command &quot; &quot; args &quot; &quot; pattern &quot; \&quot;&quot; dir &quot;\&quot;&quot;) 'ack-mode
</div><div class="parity1"><a class="lineno" href="#l40" id="l40">    40</a>                       (when ack-use-search-in-buffer-name
</div><div class="parity0"><a class="lineno" href="#l41" id="l41">    41</a>                         (function (lambda (ignore)
</div><div class="parity1"><a class="lineno" href="#l42" id="l42">    42</a>                                     ack-full-buffer-name)))
</div><div class="parity0"><a class="lineno" href="#l43" id="l43">    43</a>                       (regexp-quote pattern))))
</div><div class="parity1"><a class="lineno" href="#l44" id="l44">    44</a>
</div><div class="parity0"><a class="lineno" href="#l45" id="l45">    45</a>(provide 'ack-emacs)
</div><div class="parity1"><a class="lineno" href="#l46" id="l46">    46</a>
</div>
</pre>


<div class="logo">
<a href="http://www.selenic.com/mercurial/">
<img src="/addons/static/hglogo.png" width=75 height=90 border=0 alt="mercurial"></a>
</div>

</body>
</html>

