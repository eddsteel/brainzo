import React, { Component } from 'react';
import {
    View,
    Text,
    StyleSheet
} from 'react-native';
import { chan } from 'js-csp';

import Brainzo from './util/Brainzo';
import Receiver from './util/Receiver';
import CoolText from './components/CoolText';
import Key from './components/Key';


export default class BrainzoRemote extends Component {
    constructor(props) {
        super(props);
        var ch = chan();
        this.state = { keyChan: ch };
        var bz = new Brainzo();
        new Receiver(ch, (k) => bz.key(k));
    }

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
                    <Key chan={this.state.keyChan} sym="XF86AudioPrev" icon="skip-previous"/>
                    <Key chan={this.state.keyChan} sym="XF86AudioPlay" icon="play-arrow"/>
                    <Key chan={this.state.keyChan} sym="XF86AudioNext" icon="skip-next"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Key chan={this.state.keyChan} sym="rwd"  icon="navigate-before"/>
                    <Key chan={this.state.keyChan} sym="play" icon="code"/>
                    <Key chan={this.state.keyChan} sym="fwd"  icon="navigate-next"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Key chan={this.state.keyChan} sym="quieter" icon="volume-down"/>
                    <Key chan={this.state.keyChan} sym="XF86AudioMute" icon="volume-mute"/>
                    <Key chan={this.state.keyChan} sym="louder" icon="volume-up"/>
                  </View>
                </View>
                <View style={styles.keyblock}>
                  <View style={styles.keyblockrow}>
                    <Key chan={this.state.keyChan} sym="super+shift+f" icon="fullscreen"/>
                    <Key chan={this.state.keyChan} sym="Up" icon="arrow-upward"/>
                    <Key chan={this.state.keyChan} sym="super+w" icon="close"/>
                  </View>
                   <View style={styles.keyblockrow}>
                    <Key chan={this.state.keyChan} sym="Left" icon="arrow-back"/>
                    <Key chan={this.state.keyChan} sym="Down" icon="arrow-downward"/>
                    <Key chan={this.state.keyChan} sym="Right" icon="arrow-forward"/>
                   </View>
                   <View style={styles.keyblockrow}>
                    <Key chan={this.state.keyChan} sym="Escape" icon="launch"/>
                    <Key chan={this.state.keyChan} sym="Tab" icon="keyboard-tab"/>
                    <Key chan={this.state.keyChan} sym="Return" icon="keyboard-return"/>
                  </View>
                </View>
              </View>
              <View style={{flexDirection: 'row', margin: 0}}>
                <Key chan={this.state.keyChan} sym="super+shift+e" icon="mode-edit"/>
                <Key chan={this.state.keyChan} sym="super+shift+w" icon="music-note"/>
                <Key chan={this.state.keyChan} sym="super+shift+g" icon="movie"/>
                <Key chan={this.state.keyChan} sym="super+shift+k" icon="tv"/>
                <Key chan={this.state.keyChan} sym="super+Tab" icon="exit-to-app"/>
                <Key chan={this.state.keyChan} sym="sleep" icon="power-settings-new"/>
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
