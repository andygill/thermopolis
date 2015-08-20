
# Make a self-signing cert
#self::
#	openssl genrsa -des3 -out server.key 1024
#	openssl req -new -key server.key -out server.csr
#	cp server.key server.key.org
#	openssl rsa -in server.key.org -out server.key
#	openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt

PUBLISH_DIR=/Users/andy/Sites/thermopolis

boot::
	cabal build
	make hack
	make push

push::
	mkdir -p $(PUBLISH_DIR)/home

	cp -r content/* $(PUBLISH_DIR)
	cp -r content/.htaccess $(PUBLISH_DIR)

	cp -r content/.htpasswd $(PUBLISH_DIR)

	cp ./dist/build/thermopolis-welcome/thermopolis-welcome $(PUBLISH_DIR)/thermopolis-welcome.cgi

	cp -r content/home/.htaccess $(PUBLISH_DIR)/home
	cp ./dist/build/thermopolis-home/thermopolis-home $(PUBLISH_DIR)/home/thermopolis-home.cgi

hack::
	cp -r include/*.html .cabal-sandbox/share/x86_64-osx-ghc-7.10.2/thermopolis-0.1.0.0/include/


test::
	curl -u andy:andy -D - 'http://localhost/~andy/thermopolis/home'
