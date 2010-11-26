<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
<link rel="icon" href="/addons/static/hgicon.png" type="image/png">
<meta name="robots" content="index, nofollow" />
<link rel="stylesheet" href="/addons/static/style.css" type="text/css" />

<title>EmacsCustomisations:ack.el</title>
</head>
<body>

<div class="buttons">
<a href="/addons/log/66">changelog</a>
<a href="/addons/shortlog/66">shortlog</a>
<a href="/addons/tags">tags</a>
<a href="/addons/rev/5b737eefe5ea">changeset</a>
<a href="/addons/file/5b737eefe5ea/">files</a>
<a href="/addons/log/5b737eefe5ea/ack.el">revisions</a>
<a href="/addons/annotate/5b737eefe5ea/ack.el">annotate</a>
<a href="/addons/raw-file/5b737eefe5ea/ack.el">raw</a>
</div>

<h2>ack.el</h2>

<table>
<tr>
 <td class="metatag">changeset 66:</td>
 <td><a href="/addons/rev/5b737eefe5ea">5b737eefe5ea</a></td></tr>


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
<div class="parity0"><a class="lineno" href="#l1" id="l1">     1</a>;;; ack.el --- Use ack where you might usually use grep.
</div><div class="parity1"><a class="lineno" href="#l2" id="l2">     2</a>
</div><div class="parity0"><a class="lineno" href="#l3" id="l3">     3</a>;; Copyright (C) 2008 Philip Jackson
</div><div class="parity1"><a class="lineno" href="#l4" id="l4">     4</a>
</div><div class="parity0"><a class="lineno" href="#l5" id="l5">     5</a>;; Author: Philip Jackson &lt;phil@shellarchive.co.uk&gt;
</div><div class="parity1"><a class="lineno" href="#l6" id="l6">     6</a>;; Version: 0.4
</div><div class="parity0"><a class="lineno" href="#l7" id="l7">     7</a>
</div><div class="parity1"><a class="lineno" href="#l8" id="l8">     8</a>;; This file is not currently part of GNU Emacs.
</div><div class="parity0"><a class="lineno" href="#l9" id="l9">     9</a>
</div><div class="parity1"><a class="lineno" href="#l10" id="l10">    10</a>;; This program is free software; you can redistribute it and/or
</div><div class="parity0"><a class="lineno" href="#l11" id="l11">    11</a>;; modify it under the terms of the GNU General Public License as
</div><div class="parity1"><a class="lineno" href="#l12" id="l12">    12</a>;; published by the Free Software Foundation; either version 2, or (at
</div><div class="parity0"><a class="lineno" href="#l13" id="l13">    13</a>;; your option) any later version.
</div><div class="parity1"><a class="lineno" href="#l14" id="l14">    14</a>
</div><div class="parity0"><a class="lineno" href="#l15" id="l15">    15</a>;; This program is distributed in the hope that it will be useful, but
</div><div class="parity1"><a class="lineno" href="#l16" id="l16">    16</a>;; WITHOUT ANY WARRANTY; without even the implied warranty of
</div><div class="parity0"><a class="lineno" href="#l17" id="l17">    17</a>;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
</div><div class="parity1"><a class="lineno" href="#l18" id="l18">    18</a>;; General Public License for more details.
</div><div class="parity0"><a class="lineno" href="#l19" id="l19">    19</a>
</div><div class="parity1"><a class="lineno" href="#l20" id="l20">    20</a>;; You should have received a copy of the GNU General Public License
</div><div class="parity0"><a class="lineno" href="#l21" id="l21">    21</a>;; along with this program ; see the file COPYING.  If not, write to
</div><div class="parity1"><a class="lineno" href="#l22" id="l22">    22</a>;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
</div><div class="parity0"><a class="lineno" href="#l23" id="l23">    23</a>;; Boston, MA 02111-1307, USA.
</div><div class="parity1"><a class="lineno" href="#l24" id="l24">    24</a>
</div><div class="parity0"><a class="lineno" href="#l25" id="l25">    25</a>;;; Commentary:
</div><div class="parity1"><a class="lineno" href="#l26" id="l26">    26</a>
</div><div class="parity0"><a class="lineno" href="#l27" id="l27">    27</a>;; ack.el provides a simple compilation mode for the perl grep-a-like
</div><div class="parity1"><a class="lineno" href="#l28" id="l28">    28</a>;; ack (http://petdance.com/ack/).
</div><div class="parity0"><a class="lineno" href="#l29" id="l29">    29</a>
</div><div class="parity1"><a class="lineno" href="#l30" id="l30">    30</a>;; If `ack-guess-type' is non-nil and `ack-mode-type-map' has a
</div><div class="parity0"><a class="lineno" href="#l31" id="l31">    31</a>;; reasonable value then ack.el will try and guess what you would like
</div><div class="parity1"><a class="lineno" href="#l32" id="l32">    32</a>;; in the --type argument for ack.
</div><div class="parity0"><a class="lineno" href="#l33" id="l33">    33</a>
</div><div class="parity1"><a class="lineno" href="#l34" id="l34">    34</a>;; To install/use put ack.el in your load-path and (require 'ack) in
</div><div class="parity0"><a class="lineno" href="#l35" id="l35">    35</a>;; your initialisation file. You can then M-x ack and you're off.
</div><div class="parity1"><a class="lineno" href="#l36" id="l36">    36</a>
</div><div class="parity0"><a class="lineno" href="#l37" id="l37">    37</a>(require 'compile)
</div><div class="parity1"><a class="lineno" href="#l38" id="l38">    38</a>
</div><div class="parity0"><a class="lineno" href="#l39" id="l39">    39</a>(defvar ack-guess-type nil
</div><div class="parity1"><a class="lineno" href="#l40" id="l40">    40</a>  &quot;Setting this value to `t' will have `ack' do its best to fill
</div><div class="parity0"><a class="lineno" href="#l41" id="l41">    41</a>in the --type argument to the ack command&quot;)
</div><div class="parity1"><a class="lineno" href="#l42" id="l42">    42</a>
</div><div class="parity0"><a class="lineno" href="#l43" id="l43">    43</a>(defvar ack-command &quot;ack --nocolor --nogroup&quot;
</div><div class="parity1"><a class="lineno" href="#l44" id="l44">    44</a>  &quot;The command to be run by the ack function.&quot;)
</div><div class="parity0"><a class="lineno" href="#l45" id="l45">    45</a>
</div><div class="parity1"><a class="lineno" href="#l46" id="l46">    46</a>(defvar ack-mode-type-map
</div><div class="parity0"><a class="lineno" href="#l47" id="l47">    47</a>  '(((c++-mode) . &quot;cpp&quot;)
</div><div class="parity1"><a class="lineno" href="#l48" id="l48">    48</a>    ((c-mode) . &quot;cc&quot;)
</div><div class="parity0"><a class="lineno" href="#l49" id="l49">    49</a>    ((css-mode) . &quot;css&quot;)
</div><div class="parity1"><a class="lineno" href="#l50" id="l50">    50</a>    ((emacs-lisp-mode) . &quot;elisp&quot;)
</div><div class="parity0"><a class="lineno" href="#l51" id="l51">    51</a>    ((fortran-mode) . &quot;fortran&quot;)
</div><div class="parity1"><a class="lineno" href="#l52" id="l52">    52</a>    ((html-mode) . &quot;html&quot;)
</div><div class="parity0"><a class="lineno" href="#l53" id="l53">    53</a>    ((xml-mode nxml-mode) . &quot;xml&quot;)
</div><div class="parity1"><a class="lineno" href="#l54" id="l54">    54</a>    ((java-mode) . &quot;java&quot;)
</div><div class="parity0"><a class="lineno" href="#l55" id="l55">    55</a>    ((lisp-mode) . &quot;lisp&quot;)
</div><div class="parity1"><a class="lineno" href="#l56" id="l56">    56</a>    ((perl-mode cperl-mode) . &quot;perl&quot;))
</div><div class="parity0"><a class="lineno" href="#l57" id="l57">    57</a>  &quot;alist describing how to fill in the '--type=' argument to ack&quot;)
</div><div class="parity1"><a class="lineno" href="#l58" id="l58">    58</a>
</div><div class="parity0"><a class="lineno" href="#l59" id="l59">    59</a>(defun ack-find-type-for-mode ()
</div><div class="parity1"><a class="lineno" href="#l60" id="l60">    60</a>  (catch 'found
</div><div class="parity0"><a class="lineno" href="#l61" id="l61">    61</a>    (dolist (mode-type ack-mode-type-map)
</div><div class="parity1"><a class="lineno" href="#l62" id="l62">    62</a>      (when (member major-mode (car mode-type))
</div><div class="parity0"><a class="lineno" href="#l63" id="l63">    63</a>        (throw 'found (cdr mode-type))))))
</div><div class="parity1"><a class="lineno" href="#l64" id="l64">    64</a>
</div><div class="parity0"><a class="lineno" href="#l65" id="l65">    65</a>(defun ack-build-command ()
</div><div class="parity1"><a class="lineno" href="#l66" id="l66">    66</a>  (let ((type (ack-find-type-for-mode)))
</div><div class="parity0"><a class="lineno" href="#l67" id="l67">    67</a>    (concat ack-command
</div><div class="parity1"><a class="lineno" href="#l68" id="l68">    68</a>            (when (and ack-guess-type type)
</div><div class="parity0"><a class="lineno" href="#l69" id="l69">    69</a>              (concat &quot; --type=&quot; type)) &quot; -- &quot;)))
</div><div class="parity1"><a class="lineno" href="#l70" id="l70">    70</a>
</div><div class="parity0"><a class="lineno" href="#l71" id="l71">    71</a>(define-compilation-mode ack-mode &quot;Ack&quot;
</div><div class="parity1"><a class="lineno" href="#l72" id="l72">    72</a>  &quot;Ack compilation mode.&quot;
</div><div class="parity0"><a class="lineno" href="#l73" id="l73">    73</a>  nil)
</div><div class="parity1"><a class="lineno" href="#l74" id="l74">    74</a>
</div><div class="parity0"><a class="lineno" href="#l75" id="l75">    75</a>;;;###autoload
</div><div class="parity1"><a class="lineno" href="#l76" id="l76">    76</a>(defun ack (command-args)
</div><div class="parity0"><a class="lineno" href="#l77" id="l77">    77</a>  (interactive
</div><div class="parity1"><a class="lineno" href="#l78" id="l78">    78</a>   (list (read-from-minibuffer &quot;Run ack (like this): &quot;
</div><div class="parity0"><a class="lineno" href="#l79" id="l79">    79</a>                               (ack-build-command)
</div><div class="parity1"><a class="lineno" href="#l80" id="l80">    80</a>                               nil
</div><div class="parity0"><a class="lineno" href="#l81" id="l81">    81</a>                               nil
</div><div class="parity1"><a class="lineno" href="#l82" id="l82">    82</a>                               'ack-history)))
</div><div class="parity0"><a class="lineno" href="#l83" id="l83">    83</a>   (compilation-start command-args 'ack-mode))
</div><div class="parity1"><a class="lineno" href="#l84" id="l84">    84</a>
</div><div class="parity0"><a class="lineno" href="#l85" id="l85">    85</a>(provide 'ack)
</div>
</pre>


<div class="logo">
<a href="http://www.selenic.com/mercurial/">
<img src="/addons/static/hglogo.png" width=75 height=90 border=0 alt="mercurial"></a>
</div>

</body>
</html>

