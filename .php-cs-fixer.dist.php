<?php

$config = new PhpCsFixer\Config();
return $config
  ->setIndent("  ")
  ->setLineEnding("\n")
  ->setRules(
    array(
      '@PSR2' => true,
      'full_opening_tag' => true,
      'elseif' => false,
      'array_syntax' => array(
        'syntax' => 'long',
      ),
      'curly_braces_position' => array(
        'functions_opening_brace' => 'same_line',
        'anonymous_functions_opening_brace' => 'same_line',
        'control_structures_opening_brace' => 'same_line',
        'classes_opening_brace' => 'same_line',
        'anonymous_classes_opening_brace' => 'same_line',
      ),
      'single_quote' => array(
        'strings_containing_single_quote_chars' => true,
      ),
      'control_structure_braces' => true,
      'no_spaces_inside_parenthesis' => true,
    )
  );
