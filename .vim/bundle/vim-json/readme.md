Better JSON for VIM
===================

Distinct highlighting of keywords vs values, JSON-specific (non-JS) warnings, quote concealing.  
Customization of Jeroen Ruigrok van der Werven's [vim-json highlighting script](http://www.vim.org/scripts/script.php?script_id=1945).   
[Pathogen-friendly.](https://github.com/tpope/vim-pathogen)

Specific customizations
-----------------------

* Added distinct **highlighting** for keywords! (This is what made me start this plugin.)
* Added **concealing** of double quotes, for a minimalist [CoffeScript](http://coffeescript.org/)-inspired look. 
	* *This requires Vim 7.3+.* To disable it `:setlocal conceallevel=0` (do it permanently on the `ftplugin/json.vim` file). 
* Added **folding** of `{...}` and `[...]` blocks. To enable it `:setlocal foldmethod=syntax` (do it permanently on the `ftplugin/json.vim` file).
* **JSON-specific Warnings:**
	* Warn about *decimals* smaller than 1 that don't start with a 0 (so .1 gives a warning, it should be 0.1).
	* Warn about *comments* and *trailing semicolons*.
	* Warn about *trailing commas* after the last element of arrays or objects.

Screenshots
-----------

**No syntax coloring**: wall of text.<br>![No syntax coloring](http://farm8.staticflickr.com/7085/7370791592_fe85355c89.jpg)

**Javascript (or for that matter, json.vim) syntax coloring**: Not much improvement from no syntax coloring.<br>![Javascript syntax coloring](http://farm8.staticflickr.com/7076/7370791528_664eb2da6d.jpg)

**Better JSON syntax coloring**: a more meaningful, distinct and elegant presentation.<br>![JSON syntax coloring](http://farm8.staticflickr.com/7234/7185560283_102f6b753d.jpg)

Why use separate JSON highlighting instead of just Javascript?
--------------------------------------------------------------

Here's 2 compelling reasons:

1. **All JSON is Javascript but NOT all Javascript is JSON.** So `{property:1}` is invalid because `property` does not have double quotes around it. `{'property':1}` is also invalid, because it's single quoted while the only thing that can placate JSON is double quoting. JSON is even fuzzy enough that `{"property":.1}` is invalid, because you should have of course written `{"property":0.1}`. Also, don't even think about [having comments](http://stackoverflow.com/questions/244777/can-i-comment-a-json-file) or semicolons, you guessed it: they're invalid. The point being that your syntax highlighter should warn you about these errors, in realtime, which is something that the Javascript highlighter doesn't (because in Javacript they're not errors!).

2. **Distinct highlighting for keywords.** JSON is an extremely lightweight data format but at it's core lies an inescapable conceptual distinction: there are keywords and then there are values. There's nothing much to the format besides that, so we might as well display keywords and values differently. This is something that gets lost with Javascript-inspired syntax highlighters, which see keywords as just another string since JSON requires them double quoted. JSON files become an impenetrable, indistinct wall of text.
