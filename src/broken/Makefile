aenea-debug-linux: common-aenea common-aenea-linux
	gprmake -Paenea-crawler/aenea.gpr -XBuild=Debug -XPlatform=Linux

aenea-mprofile-linux: common-aenea common-aenea-linux
	gprmake -Paenea-crawler/aenea.gpr -XBuild=Memory_Profile -XPlatform=Linux

aenea-release-linux: common-aenea common-aenea-linux
	gprmake -Paenea-crawler/aenea.gpr -XBuild=Release -XPlatform=Linux

aenea-debug-win: common-aenea common-aenea-win
	gprmake -Paenea-crawler/aenea.gpr -XBuild=Debug -XPlatform=Windows

aenea-profile-win: common-aenea common-aenea-win
	gprmake -Paenea-crawler/aenea.gpr -XBuild=Profile -XPlatform=Windows

aenea-release-win: common-aenea common-aenea-win
	gprmake -Paenea-crawler/aenea.gpr -XBuild=Release -XPlatform=Windows

# EDIT THIS FOLLOWING ENTRY IN THE PRODUCTION ENVIRONMNET
aenea-production: aenea-debug-linux
	updateaa
	strip /home/mosteo/bin/aenea/aenea

common-aenea-linux:
	cd obj && ln -s -f /usr/lib/libpq.so.3.1 libpq.so

common-aenea-win:
	cp ../win32/libpq.a obj
	
common-aenea:
	mkdir -p obj
	mkdir -p log
	make -C ../aws/ssl build
	cp ../aws/ssl/libnosslaws.a obj

clean:
	rm -rf obj log ../aws/ssl/*.ali ../aws/ssl/*.a ../aws/ssl/*.o
