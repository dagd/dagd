<?php
final class DaGdNewHelpController extends DaGdController {
  public static function getHelp() {
    return id(new DaGdHelp())
      ->setTitle('help')
      ->setDescription(
        'Provides a list of valid commands and examples of how to use them')
      ->addWarning('Information here may be incomplete')
      ->addPath(
        id(new DaGdHelpPath())
          ->setPath('newhelp')
          ->setMethods(array('GET')));
  }

  public function getStyle() {
    $style = <<<EOD
.apphelp {
  width: 700px;
  margin: 15px auto;
  border: 1px solid #ccc;
}
.apptitle {
  background-color: #ddd;
  padding: 10px;
  color: #666;
}
.helppad { padding: 0 10px; }
.column {
  width: 310px;
  float: left;
}
.topborder {
  clear: both;
  border-top: 1px solid #ccc;
}
EOD;

    return array_merge(parent::getStyle(), array($style));
  }

  private function helpHtml($ctrl) {
    if (!is_subclass_of($ctrl, 'DaGdController')) {
      // Curtail warnings caused by trying to statically call getHelp
      // when old-style controllers didn't expose it as a static method.
      return null;
    }

    $help = call_user_func(array($ctrl, 'getHelp'));

    // We only support the new help system here.
    if (!($help instanceof DaGdHelp)) {
      return null;
    }

    $title = tag(
      'div',
      'Application: '.$help->getTitle(),
      array(
        'class' => 'apptitle',
      )
    );

    $summary = tag(
      'div',
      array(
        tag('h3', 'Summary'),
        tag('p', $help->getDescription()),
      ),
      array(
        'class' => 'column helppad',
      )
    );

    // Examples are children of paths, so we handle both here
    // so that we only have to iterate paths once. We also handle the
    // documentation for get-args.

    $paths = array(
      tag('h3', 'Paths'),
    );

    $examples = array(
      tag('h3', 'Examples'),
    );

    foreach ($help->getPaths() as $path) {
      $get_args_tbl = array();

      if ($path->getGetArgs()) {
        $get_args_tbl[] = tag('h5', 'GET parameters');
        $arg_trs = array();
        foreach ($path->getGetArgs() as $arg => $desc) {
          $arg_trs[] = tag(
            'tr',
            array(
              tag('td', tag('code', $arg.': ')),
              tag('td', $desc),
            )
          );
        }

        $get_args_tbl[] = tag('table', $arg_trs);
      }

      $paths[] = tag(
        'div',
        array(
          tag('pre', $path->render()),
          $get_args_tbl,
        )
      );

      // Attempt to iterate over examples if we have any
      foreach ($path->getExamples() as $example) {
        $commentary = null;
        if ($example->getCommentary()) {
          $commentary = tag('small', $example->getCommentary());
        }
        $examples[] = tag(
          'div',
          array(
            $commentary,
            tag(
              'pre',
              "\$ curl '".$example->render()."'",
              array(
                'class' => 'examplepre',
                'style' => 'margin-left: 20px;',
              )
            ),
          )
        );
      }

      // But we might not have any. Make one anyway.
      if (!$path->getExamples()) {
        $baseurl = DaGdConfig::get('general.baseurl');
        $url = $baseurl.'/'.$path->getPath();
        $examples[] = tag(
          'pre',
          "\$ curl '".$url."'",
          array(
            'class' => 'examplepre',
          )
        );
      }
    }

    $paths_element = tag(
      'div',
      $paths,
      array(
        'class' => 'column helppad',
      )
    );

    $examples_element = tag(
      'div',
      $examples,
      array(
        'class' => 'helppad topborder',
      )
    );

    // On to warnings
    $warnings = null;
    if ($help->getWarnings()) {
      $warnings_ul = tag('ul', array());
      foreach ($help->getWarnings() as $warning) {
        $warnings_ul->addTag(tag('li', $warning));
      }

      $warning_titles = array(
        'Words to the wise:',
        'Cautionary tales:',
        'Words of wisdom:',
        'You might wish to read this:',
        'A few notes:',
        'Some advice:',
        'Travel tips:',
        'Admonition:',
      );
      $warnings = tag(
        'div',
        array(
          tag('h3', $warning_titles[array_rand($warning_titles)]),
          $warnings_ul,
        ),
        array(
          'class' => 'helppad topborder',
        )
      );
    }

    // Compose everything together
    $block = tag(
      'div',
      array(
        $title,
        $summary,
        $paths_element,
        $examples_element,
        $warnings,
      ),
      array(
        'class' => 'apphelp',
      )
    );

    return $block;
  }

  public function render(DaGdHtmlResponse $response) {
    $routes = DaGdConfig::get('general.routemap');
    $help_blocks = array();
    $controllers_visited = array();

    foreach ($routes as $path => $metadata) {
      if (in_array($metadata['controller'], $controllers_visited)) {
        continue;
      }
      $help = $this->helpHtml($metadata['controller']);
      if ($help) {
        $help_blocks[] = $help;
      }
      $controllers_visited[] = $metadata['controller'];
    }

    $template = $this
      ->getBaseTemplate()
      ->setBody($help_blocks)
      ->setStyle($this->getStyle())
      ->setTitle('help')
      ->getHtmlTag();

    return $response->setBody($template);
  }
}
