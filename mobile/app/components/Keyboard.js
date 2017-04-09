import React, { Component } from 'react';
import { putAsync } from 'js-csp';
import {
    View,
    Text,
    StyleSheet,
    TextInput,
    Button,
} from 'react-native';

import Key from './Key';

const styles = {
    keyblock: {
        margin: 10,
        padding: 5,
    },
    keyblockrow: {
        flexDirection: 'row',
        padding: 0,
        margin: 0,
    }
};

export default class Keyboard extends Component {
    constructor(props) {
        super(props);
        this.state = {text: ''};
    }

    sendKeys() {
        var str = this.state.text;
        var transform = function(c) {
            if (c == ' ') {
                return 'space';
            } else {
                return c;
            };
        }
        var chars = Array.from(str).map(transform);
        chars.map((c) => putAsync(this.props.chan, c));
        this.state = {text: ''};
    }

    render() {
        var ch = this.props.chan;
        return (
            <View style={{alignItems: 'center'}}>
              <View style={{flexDirection: 'row'}}>
                <TextInput
                  style={{height: 40, flex: 17, margin: 5}}
                  placeholder="enter keys"
                  onChangeText={(text) => this.setState({text})}
                  onSubmitEditing={() => this.sendKeys()}
                  value={this.state.value}/>
              </View>
              <View style={styles.keyblockrow}>
                <View style={styles.keyblock}>
                  <View style={styles.keyblockrow}>
                    <Key chan={ch} sym="XF86AudioPrev" icon="skip-previous"/>
                    <Key chan={ch} sym="XF86AudioPlay" icon="play-arrow"/>
                    <Key chan={ch} sym="XF86AudioNext" icon="skip-next"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Key chan={ch} sym="rwd"  icon="navigate-before"/>
                    <Key chan={ch} sym="play" icon="code"/>
                    <Key chan={ch} sym="fwd"  icon="navigate-next"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Key chan={ch} sym="quieter" icon="volume-down"/>
                    <Key chan={ch} sym="XF86AudioMute" icon="volume-mute"/>
                    <Key chan={ch} sym="louder" icon="volume-up"/>
                  </View>
                </View>
                <View style={styles.keyblock}>
                  <View style={styles.keyblockrow}>
                    <Key chan={ch} sym="super+shift+f" icon="fullscreen"/>
                    <Key chan={ch} sym="Up" icon="arrow-upward"/>
                    <Key chan={ch} sym="super+w" icon="close"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Key chan={ch} sym="Left" icon="arrow-back"/>
                    <Key chan={ch} sym="Down" icon="arrow-downward"/>
                    <Key chan={ch} sym="Right" icon="arrow-forward"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Key chan={ch} sym="Escape" icon="launch"/>
                    <Key chan={ch} sym="Tab" icon="keyboard-tab"/>
                    <Key chan={ch} sym="Return" icon="keyboard-return"/>
                  </View>
                </View>
              </View>
              <View style={styles.keyblockrow}>
                <Key chan={ch} sym="super+shift+e" icon="mode-edit"/>
                <Key chan={ch} sym="super+shift+w" icon="music-note"/>
                <Key chan={ch} sym="super+shift+g" icon="movie"/>
                <Key chan={ch} sym="super+shift+k" icon="tv"/>
                <Key chan={ch} sym="super+Tab" icon="exit-to-app"/>
                <Key chan={ch} sym="sleep" icon="power-settings-new"/>
              </View>
            </View>
        );
    }
}
