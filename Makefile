ANDROID_HOME:=/opt/android-sdk

emulator:
	/opt/android-sdk/tools/emulator @reactivenexus5

run:
	cd mobile && \
	react-native run-android

release: mobile/brainzo.apk

mobile/brainzo.apk: build sign

build:
	cd mobile &&\
	react-native bundle --platform=android --dev=false --entry-file=index.js --bundle-output android/app/src/main/assets/index.android.bundle --assets-dest android/app/src/main/res/  &&\
	cd android &&\
	./gradlew clean bundleRelease assembleRelease

sign:
	cd mobile/android/app/build/outputs/apk/release && \
	jarsigner -keystore ~/.secrets/android.keystore app-release-unsigned.apk edd && \
	mv app-release-unsigned.apk ../../../../../../brainzo.apk

install: mobile/brainzo.apk
	adb install -r mobile/brainzo.apk

install-service:
	sudo install $(shell stack exec -- which brainzo-api) /usr/bin
	sudo install brainzo.service /usr/lib/systemd/user/brainzo.service
	systemctl --user daemon-reload

log:
	adb logcat *:S ReactNative:V ReactNativeJS:V
