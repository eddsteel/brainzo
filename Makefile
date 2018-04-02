ANDROID_HOME:=/opt/android-sdk

emulator:
	/opt/android-sdk/tools/emulator @reactivenexus5

run:
	cd mobile && \
	react-native run-android

release: mobile/brainzo.apk

mobile/brainzo.apk:
	cd mobile/android && \
	./gradlew assembleRelease && \
	cd app/build/outputs/apk && \
	jarsigner -keystore ~/.secrets/android.keystore app-release-unsigned.apk edd && \
	mv app-release-unsigned.apk ../../../../../brainzo.apk

install: mobile/brainzo.apk
	adb install -r mobile/brainzo.apk

log:
	adb logcat *:S ReactNative:V ReactNativeJS:V
