import React, { Component } from 'react';
import {
    View,
    Text,
    StyleSheet
} from 'react-native';

import Key from './components/Key';
import CoolText from './components/CoolText';

export default class BrainzoRemote extends Component {
    render() {
        return (
            <View style={styles.container}>
              <CoolText
                style={styles.brainzo}
                titleColors={['#ff00dd', '#00ffdd', '#ddff00']}
                letters={['B', 'R', 'A', 'I', 'N', 'Z', 'O', '!']} />
              <Text style={styles.welcome}>
                Bleep Bloop.
              </Text>

              <View style={{flexDirection: 'row', margin: 0}}>
                <View style={styles.keyblock}>
                  <View style={styles.keyblockrow}>
                    <Key sym="XF86AudioPrev" icon="skip-previous"/>
                    <Key sym="XF86AudioPlay" icon="play-arrow"/>
                    <Key sym="XF86AudioNext" icon="skip-next"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Key sym="rwd"  icon="navigate-before"/>
                    <Key sym="play" icon="code"/>
                    <Key sym="fwd"  icon="navigate-next"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Key sym="quieter" icon="volume-down"/>
                    <Key sym="XF86AudioMute" icon="volume-mute"/>
                    <Key sym="louder" icon="volume-up"/>
                  </View>
                </View>
                <View style={styles.keyblock}>
                  <View style={styles.keyblockrow}>
                    <Key sym="super+shift+f" icon="fullscreen"/>
                    <Key sym="Up" icon="arrow-upward"/>
                    <Key sym="super+w" icon="close"/>
                  </View>
                   <View style={styles.keyblockrow}>
                    <Key sym="Left" icon="arrow-back"/>
                    <Key sym="Down" icon="arrow-downward"/>
                    <Key sym="Right" icon="arrow-forward"/>
                   </View>
                   <View style={styles.keyblockrow}>
                    <Key sym="Escape" icon="launch"/>
                    <Key sym="Tab" icon="keyboard-tab"/>
                    <Key sym="Return" icon="keyboard-return"/>
                  </View>
                </View>
              </View>
              <View style={{flexDirection: 'row', margin: 0}}>
                <Key sym="super+shift+e" icon="mode-edit"/>
                <Key sym="super+shift+w" icon="music-note"/>
                <Key sym="super+shift+g" icon="movie"/>
                <Key sym="super+shift+k" icon="tv"/>
                <Key sym="super+Tab" icon="exit-to-app"/>
                <Key sym="sleep" icon="power-settings-new"/>
              </View>
            </View>
        );
    }
}

const styles = StyleSheet.create({
    container: {
        flex: 1,
        justifyContent: 'center',
        alignItems: 'center',
        backgroundColor: '#F5FCFF',
    },
    welcome: {
        fontSize: 20,
        textAlign: 'center',
        margin: 10,
        color: '#ff00dd',
    },
    brainzo: {
        fontSize: 30,
        textAlign: 'center',
        margin: 10,
    },
    keyblock: {
        margin: 10,
        padding: 5,
    },
    keyblockrow: {
        flexDirection: 'row',
        padding: 0,
        margin: 0,
    }
});
