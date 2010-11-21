<?php
function render()
{
  $html =<<<HTML
              <head>
              </head>
              <body>
                <div id="some_id" class="some_class"></div>
                <div id="some_id" class="some_class"></div>
                <form method="post" id="" action="">
                </form>
                <form method="get" id="id2" action="do.php">
                </form>
              </body>
HTML;

echo $html;
}

render();
              ?>
