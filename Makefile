
# Make a self-signing cert
self::
	openssl genrsa -des3 -out server.key 1024
	openssl req -new -key server.key -out server.csr
	cp server.key server.key.org
	openssl rsa -in server.key.org -out server.key
	openssl x509 -req -days 365 -in server.csr -signkey server.key -out server.crt

PUBLISH_DIR=/Users/andy/Sites/thermopolis

push::
	cp -r content/* $(PUBLISH_DIR)
	cp -r content/.htaccess $(PUBLISH_DIR)

	cp ./dist/build/thermopolis-welcome/thermopolis-welcome $(PUBLISH_DIR)/thermopolis-welcome.cgi
#	cp ./dist/build/thermopolis/thermopolis-home    $(PUBLISH_DIR)/	
