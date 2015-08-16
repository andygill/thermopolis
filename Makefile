
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
	cp ./dist/build/thermopolis/thermopolis $(PUBLISH_DIR)/welcome.cgi
