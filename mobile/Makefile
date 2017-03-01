ANDROID_HOME:=/opt/android-sdk

emulator:
	/opt/android-sdk/tools/emulator @reactivenexus5

run:
	react-native run-android

release: brainzo.apk

brainzo.apk:
	cd android && \
	./gradlew assembleRelease && \
	cd app/build/outputs/apk && \
	jarsigner -keystore ~/.secrets/android.keystore app-release-unsigned.apk edd && \
	mv app-release-unsigned.apk ../../../../../brainzo.apk

install: brainzo.apk
	adb install -r brainzo.apk
