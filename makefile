zip:
	mv notify notify.bak
	mkdir -p ./notify/notify
	cp -rf notify.bak/* ./notify/notify/
	cp notify.bat ./notify/
	rm ./notify/notify/build.bat;
	rm ./notify/notify/notify.wlua; 
	rm ./notify/notify/wsrlua.exe; 
	rm ./notify/notify/glue.exe; 
	zip -ru notify.zip notify;
	rm -rf notify
	mv notify.bak notify
