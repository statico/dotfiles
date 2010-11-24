<?php

/*
 * This file is part of the symfony package.
 * (c) 2004-2006 Fabien Potencier <fabien.potencier@symfony-project.com>
 * (c) 2004 David Heinemeier Hansson
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * TextHelper.
 *
 * @package    symfony
 * @subpackage helper
 * @author     Fabien Potencier <fabien.potencier@symfony-project.com>
 * @author     David Heinemeier Hansson
 * @version    SVN: $Id: TextHelper.php 3699 2007-04-02 11:47:32Z fabien $
 */

/**
 * Truncates +text+ to the length of +length+ and replaces the last three characters with the +truncate_string+
 * if the +text+ is longer than +length+.
 */
function truncate_text($text, $length = 30, $truncate_string = '...', $truncate_lastspace = false)
{
  if ($text == '')
  {
    return '';
  }

  if (strlen($text) > $length)
  {
    $truncate_text = substr($text, 0, $length - strlen($truncate_string));
    if ($truncate_lastspace)
    {
      $truncate_text = preg_replace('/\s+?(\S+)?$/', '', $truncate_text);
    }

    return $truncate_text.$truncate_string;
  }
  else
  {
    return $text;
  }
}

/**
 * Highlights the +phrase+ where it is found in the +text+ by surrounding it like
 * <strong class="highlight">I'm a highlight phrase</strong>. The highlighter can be specialized by
 * passing +highlighter+ as single-quoted string with \1 where the phrase is supposed to be inserted.
 * N.B.: The +phrase+ is sanitized to include only letters, digits, and spaces before use.
 */
function highlight_text($text, $phrase, $highlighter = '<strong class="highlight">\\1</strong>')
{
  if ($text == '')
  {
    return '';
  }

  if ($phrase == '')
  {
    return $text;
  }

  return preg_replace('/('.preg_quote($phrase, '/').')/i', $highlighter, $text);
}

/**
 * Extracts an excerpt from the +text+ surrounding the +phrase+ with a number of characters on each side determined
 * by +radius+. If the phrase isn't found, nil is returned. Ex:
 *   excerpt("hello my world", "my", 3) => "...lo my wo..."
 */
function excerpt_text($text, $phrase, $radius = 100, $excerpt_string = '...')
{
  if ($text == '' || $phrase == '')
  {
    return '';
  }

  $found_pos = strpos(strtolower($text), strtolower($phrase));
  if ($found_pos !== false)
  {
    $start_pos = max($found_pos - $radius, 0);
    $end_pos = min($found_pos + strlen($phrase) + $radius, strlen($text));

    $prefix = ($start_pos > 0) ? $excerpt_string : '';
    $postfix = $end_pos < strlen($text) ? $excerpt_string : '';

    return $prefix.substr($text, $start_pos, $end_pos - $start_pos).$postfix;
  }
}

/**
 * Word wrap long lines to line_width.
 */
function wrap_text($text, $line_width = 80)
{
  return preg_replace('/(.{1,'.$line_width.'})(\s+|$)/s', "\\1\n", preg_replace("/\n/", "\n\n", $text));
}

/*
    # Returns +text+ transformed into html using very simple formatting rules
    # Surrounds paragraphs with <tt>&lt;p&gt;</tt> tags, and converts line breaks into <tt>&lt;br /&gt;</tt>
    # Two consecutive newlines(<tt>\n\n</tt>) are considered as a paragraph, one newline (<tt>\n</tt>) is
    # considered a linebreak, three or more consecutive newlines are turned into two newlines
*/
function simple_format_text($text, $options = array())
{
  $css = (isset($options['class'])) ? ' class="'.$options['class'].'"' : '';

  $text = sfToolkit::pregtr($text, array("/(\r\n|\r)/"        => "\n",               // lets make them newlines crossplatform
                                         "/\n{3,}/"           => "\n\n",             // zap dupes
                                         "/\n\n/"             => "</p>\\0<p$css>",   // turn two newlines into paragraph
                                         "/([^\n])\n([^\n])/" => "\\1\n<br />\\2")); // turn single newline into <br/>

  return '<p'.$css.'>'.$text.'</p>'; // wrap the first and last line in paragraphs before we're done
}

/**
 * Turns all urls and email addresses into clickable links. The +link+ parameter can limit what should be linked.
 * Options are :all (default), :email_addresses, and :urls.
 *
 * Example:
 *   auto_link("Go to http://www.symfony-project.com and say hello to fabien.potencier@example.com") =>
 *     Go to <a href="http://www.symfony-project.com">http://www.symfony-project.com</a> and
 *     say hello to <a href="mailto:fabien.potencier@example.com">fabien.potencier@example.com</a>
 */
function auto_link_text($text, $link = 'all', $href_options = array())
{
  if ($link == 'all')
  {
    return _auto_link_urls(_auto_link_email_addresses($text), $href_options);
  }
  else if ($link == 'email_addresses')
  {
    return _auto_link_email_addresses($text);
  }
  else if ($link == 'urls')
  {
    return _auto_link_urls($text, $href_options);
  }
}

/*
 * Turns all links into words, like "<a href="something">else</a>" to "else".
 */
function strip_links_text($text)
{
  return preg_replace('/<a.*>(.*)<\/a>/m', '\\1', $text);
}

if (!defined('SF_AUTO_LINK_RE'))
{
  define('SF_AUTO_LINK_RE', '~
    (                       # leading text
      <\w+.*?>|             #   leading HTML tag, or
      [^=!:\'"/]|           #   leading punctuation, or
      ^                     #   beginning of line
    )
    (
      (?:https?://)|        # protocol spec, or
      (?:www\.)             # www.*
    )
    (
      [-\w]+                   # subdomain or domain
      (?:\.[-\w]+)*            # remaining subdomains or domain
      (?::\d+)?                # port
      (?:/(?:(?:[\~\w\+%-]|(?:[,.;:][^\s$]))+)?)* # path
      (?:\?[\w\+%&=.;-]+)?     # query string
      (?:\#[\w\-]*)?           # trailing anchor
    )
    ([[:punct:]]|\s|<|$)    # trailing text
   ~x');
}

/**
 * Turns all urls into clickable links.
 */
function _auto_link_urls($text, $href_options = array())
{
  $href_options = _tag_options($href_options);
  return preg_replace_callback(
    SF_AUTO_LINK_RE,
    create_function('$matches', '
      if (preg_match("/<a\s/i", $matches[1]))
      {
        return $matches[0];
      }
      else
      {
        return $matches[1].\'<a href="\'.($matches[2] == "www." ? "http://www." : $matches[2]).$matches[3].\'"'.$href_options.'>\'.$matches[2].$matches[3].\'</a>\'.$matches[4];
      }
    ')
  , $text);
}

/**
 * Turns all email addresses into clickable links.
 */
function _auto_link_email_addresses($text)
{
  return preg_replace('/([\w\.!#\$%\-+.]+@[A-Za-z0-9\-]+(\.[A-Za-z0-9\-]+)+)/', '<a href="mailto:\\1">\\1</a>', $text);
}
