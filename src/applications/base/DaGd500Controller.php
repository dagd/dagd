<?php

final class DaGd500Controller extends DaGdErrorController {
  private $exception;

  private function setup($response) {
    statsd_bump('status,code=500');
    $response->setCode(500);
  }

  public function setException($ex) {
    $this->exception = $ex;
  }

  public function render(DaGdHTMLResponse $response) {
    $this->setup($response);

    $responses = array(
      'This action has successfully failed.',
      'Well, this is just not good.',
      'Something has happened. Something bad. Really bad.',
      'This page died. Just like my hopes and dreams.',
      'Sorry, I\'m not feeling well right now.',
    );

    $exception_tag = null;
    $want_trace = DaGdConfig::get('general.display_errors');
    if ($this->exception && $want_trace) {
      $exception_tag = tag(
        'div',
        array(
          tag('hr'),
          tag(
            'h4',
            'Debugging information follows (general.display_errors = true):'),
          tag('p', $this->exception->getMessage()),
          tag('pre', $this->exception->getTraceAsString())
        ),
        array(
          'style' => 'text-align: left;',
        )
      );
    }

    $default = $responses[array_rand($responses)];
    $message = $this->getMessage($default);

    $text = tag(
      'div',
      array(
        tag('h1', $message),
        tag('h2', '500.'),
        tag('h3', 'Internal server error.'),
        $this->getHtmlMessage(),
        $exception_tag,
      ),
      array(
        'style' => 'text-align: center;',
      )
    );

    $bigcloud = tag(
      'div',
      '🤒',
      array(
        'style' => 'font-size: 6em; text-align: center; margin-top: 20px;',
      )
    );

    $template = $this
      ->getBaseTemplate()
      ->setBody(array($bigcloud, $text))
      ->setStyle($this->getStyle())
      ->setTitle('500')
      ->getHtmlTag();
    return $response->setBody($template);
  }

  public function execute(DaGdResponse $response) {
    $this->setup($response);

    $trace = '';
    $want_trace = DaGdConfig::get('general.display_errors');
    if ($this->exception && $want_trace) {
      $trace = "\n".$this->exception->getMessage();
      $trace .= "\n".$this->exception->getTraceAsString();
    }
    return $this->getMessage('500 - internal server error').$trace;
  }
}
