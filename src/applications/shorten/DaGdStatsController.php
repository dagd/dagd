<?php

final class DaGdStatsController extends DaGdController {
  public static function getHelp() {
    return array(
      'title' => 'stats',
      'summary' => 'Display basic stats for a short url.',
      'path' => 'stats',
      'examples' => array(
        array(
          'arguments' => array('g'),
          'summary' => 'An example short URL with a custom suffix'),
      ));
  }

  public function getStyle() {
    $url = DaGdStaticController::url('img/loader.gif');
    $style = <<<EOD
#access_graph { border: 1px solid #ddd; }
.loading { background: transparent url($url) center no-repeat; }
EOD;

    return array_merge(
      parent::getStyle(),
      array($style)
    );
  }

  public function execute(DaGdResponse $response) {
    $shorturl = $this->getRequest()->getRouteComponent(1);
    $query = new DaGdShortURLQuery($this);
    if ($query->fromShort($shorturl)) {
      return 'text-based stats endpoint coming soon';
    } else {
      return $this->error(404)->execute($response);
    }
  }

  public function render(DaGdHTMLResponse $response) {
    $shorturl = $this->getRequest()->getRouteComponent(1);

    $query = new DaGdShortURLQuery($this);

    // Daily access graph
    $accesses = $query->dailyAccess($shorturl, 60);

    if (count($accesses) == 0) {
      return $this->error(404)->finalize();
    }

    $dates = array();
    $counts = array();
    foreach ($accesses as $date => $count) {
      $dates[] = $date;
      $counts[] = $count;
    }
    $dates = implode(',', $dates);
    $counts = implode(',', $counts);

    $h1 = tag('h1', 'Statistics for '.$shorturl);

    $demo_graph = <<<EOD
function getGraphWidth() {
  return {
    width: document.getElementById('access_graph').offsetWidth,
    height: 240,
  }
}

function makeChart() {
  console.time('chart');
  let opts = {
    title: "Accesses, last 60 days",
    width: document.getElementById('access_graph').offsetWidth,
    height: 240,
    scales: {
      x: {
      },
    },
    series: [
      {},
      {
        stroke: "green",
      },
    ],
    axes: [
      {},
      {
        label: 'Accesses',
      },
    ],
    cursor: {
      show: false
    },
    select: {
      show: false,
    },
    legend: {
      show: false,
    },
  };

  const data = [
    [$dates],
    [$counts],
  ];

  let u = new uPlot(opts, data, document.getElementById('access_graph'));

  function throttle(cb, limit) {
    var wait = false;
    return function() {
      if (!wait) {
        requestAnimationFrame(cb);
        wait = true;
        setTimeout(function() {
          wait = false;
        }, limit);
      }
    }
  }
  window.addEventListener("resize", throttle(function() { u.setSize(getGraphWidth()); }, 100));
  console.timeEnd('chart');
}

document.addEventListener('DOMContentLoaded', function(event) {
  makeChart();
});
EOD;

    $want_screenshots = DaGdConfig::get('shorten.stats_screenshots');
    $screenshot = null;
    if ($want_screenshots) {
      $screenshot = tag(
        'img',
        null,
        array(
          'alt' => 'screenshot',
          'src' => '/screenshot/'.$shorturl,
          'class' => 'loading',
          'height' => '240',
          'width' => '320',
          'style' => 'float: left;',
        )
      );
    }

    $body = tag(
      'div',
      array(
        $h1,
        $screenshot,
        tag('div', '', array('id' => 'access_graph', 'style' => 'float: right; width: 65%;')),
        tag('script', $demo_graph, array('type' => 'text/javascript'), true),
      )
    );

    $template = $this
      ->getBaseTemplate()
      ->addStylesheet('uPlot/uPlot.min.css')
      ->addJavascript('uPlot/uPlot.iife.min.js')
      ->setBody($body)
      ->getHtmlTag();

    return $response->setBody($template);
  }
}
