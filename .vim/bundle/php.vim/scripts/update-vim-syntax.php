<?php
/**
 * Script to update Vim's PHP syntax file.
 *
 * @author Stan Angeloff <stanimir@angeloff.name>
 *
 * @author Paul Garvin <paul@paulgarvin.net>
 * @copyright Copyright 2009 Paul Garvin
 * @license http://www.opensource.org/licenses/mit-license.php MIT License
 *
 * Loosely based on Paul Garvin <paul@paulgarvin.net> original script.
 *
 * For the full copyright and license information,
 * please view the LICENSE file that was distributed with this source code.
 */

# Housekeeping.
error_reporting(E_ALL | E_DEPRECATED | E_STRICT);
ini_set('display_errors', 'On');

date_default_timezone_set('UTC');

$blocks = array(
    'extensions' => array(),
    'last-modified' => sprintf(
        '" %s, PHP %s',
        date('r' /* RFC 2822 */),
        PHP_VERSION
    ),
);

# Parse the configuration file associated with this script.
$configuration = parse_ini_file(
    __DIR__ . '/syntax.ini',
    /* $process_sections = */ true
);

# Process extensions and generate built-in functions, constants, classes and interfaces.
$extensions = array();

foreach ($configuration['extensions'] as $extensionName => $isEnabled) {
    if (( ! $isEnabled)) {
        continue;
    }

    try {
        $reflect = new ReflectionExtension($extensionName);

        $options = array(
            'name' => $reflect->getName(),
            'classes' => array(),
            'functions' => array_keys($reflect->getFunctions()),
            'constants' => array_diff(
                array_keys($reflect->getConstants()),
                array('TRUE', 'FALSE', 'NULL')
            ),
        );

        foreach ($reflect->getClasses() as $extensionClass) {
            $options['classes'][] = $extensionClass->getName();
            $options['constants'] = array_unique(
                array_merge(
                    $options['constants'],
                    array_keys($extensionClass->getConstants())
                )
            );
        }

        sort($options['classes'], SORT_NATURAL);
        sort($options['functions'], SORT_NATURAL);
        sort($options['constants'], SORT_NATURAL);

        $extensions[$extensionName] = $options;

    } catch (ReflectionException $e) {
        file_put_contents(
            'php://stderr',
            sprintf(
                '[ERROR] %s: %s.' . PHP_EOL,
                $extensionName,
                rtrim($e->getMessage(), ' ?!.')
            )
        );
    }
}

$blocks['extensions'][] = 'if ! exists("g:php_syntax_extensions_enabled")';
$blocks['extensions'][] = sprintf(
    '    let g:php_syntax_extensions_enabled = ["%s"]',
    implode(
        /* $glue = */ '", "',
        array_map(
            'strtolower',
            array_keys($extensions)
        )
    )
);
$blocks['extensions'][] = 'endif';

$blocks['extensions'][] = 'if ! exists("g:php_syntax_extensions_disabled")';
$blocks['extensions'][] = '    let g:php_syntax_extensions_disabled = []';
$blocks['extensions'][] = 'endif';

$ifExtensionEnabled = function ($extensionName) {
    return sprintf(
        'if ' .
        'index(g:php_syntax_extensions_enabled, "%1$s") >= 0 && ' .
        'index(g:php_syntax_extensions_disabled, "%1$s") < 0 && ' .
        '( ! exists("b:php_syntax_extensions_enabled") || index(b:php_syntax_extensions_enabled, "%1$s") >= 0) && ' .
        '( ! exists("b:php_syntax_extensions_disabled") || index(b:php_syntax_extensions_disabled, "%1$s") < 0)',
        strtolower($extensionName)
    );
};

$blocks['extensions'][] = 'syn case match';

foreach ($extensions as $extension) {
    if (( ! sizeof($extension['constants']))) {
        continue;
    }

    $blocks['extensions'][] = $ifExtensionEnabled($extension['name']);

    $blocks['extensions'][] = sprintf('" %s constants', $extension['name']);
    $blocks['extensions'][] = sprintf(
        'syn keyword phpConstants %s contained',
        implode(/* $glue = */ ' ', $extension['constants'])
    );

    $blocks['extensions'][] = 'endif';
}

$blocks['extensions'][] = 'syn case ignore';

foreach ($extensions as $extension) {
    if (( ! sizeof($extension['functions']) && ! sizeof($extension['classes']))) {
        continue;
    }

    $blocks['extensions'][] = $ifExtensionEnabled($extension['name']);

    if (sizeof($extension['functions'])) {
        $blocks['extensions'][] = sprintf('" %s functions', $extension['name']);
        $blocks['extensions'][] = sprintf(
            'syn keyword phpFunctions %s contained',
            implode(/* $glue = */ ' ', $extension['functions'])
        );
    }
    if (sizeof($extension['classes'])) {
        $blocks['extensions'][] = sprintf('" %s classes and interfaces', $extension['name']);
        $blocks['extensions'][] = sprintf(
            'syn keyword phpClasses %s contained',
            implode(/* $glue = */ ' ', $extension['classes'])
        );
    }

    $blocks['extensions'][] = 'endif';
}

# Read the existing syntax file with block markers in it.
#
$template = file_get_contents('/var/php/syntax/php.vim');

# Clean up any previously defined blocks.
$template = preg_replace(
    sprintf(
        '/
            ^  # start of a line
                ("\s*{{{\s*block:\s*)  # look for the beginning of a block
                (%s)\b  # a known block name next to a word boundary
                .*?  # dot matches any character, including newline, non-greedy
                ("\s*}}})  # look for the end of a block
            $  # end of a line
        /ismx',
        implode(
            /* $glue = */ '|',
            array_keys($blocks)
        )
    ),
    '\1\2' . PHP_EOL . PHP_EOL . '{{{\2}}}' . PHP_EOL . PHP_EOL . '\3',
    $template
);

# Update block contents in the template.
foreach ($blocks as $blockName => $blockLines) {
    $template = str_ireplace(
        sprintf('{{{%s}}}', $blockName),
        rtrim(
            is_array($blockLines) ? implode(PHP_EOL, $blockLines) : $blockLines
        ),
        $template
    );
}

print $template;
