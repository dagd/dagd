This is what my development sandbox's /etc/httpd/conf.d/dagd.local.conf looks like.

    <VirtualHost *:80>
      # This file is loosely based on the recommended Phabricator apache config.
      ServerName dagd.local
    
      DocumentRoot /home/ricky/devel/da.gd/src/webroot/
    
      RewriteEngine on
      RewriteRule ^(.*)$          /index.php?__path__=$1  [L,QSA]
    </VirtualHost>
