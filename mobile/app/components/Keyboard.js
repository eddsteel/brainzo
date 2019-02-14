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
import Mouse from './Mouse';

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
        this.setState({text: ''});
    }

    render() {
        var ch = this.props.chan;
        var mc = this.props.mchan;
        return (
            <View style={{alignItems: 'center'}}>
              <View style={{flexDirection: 'row'}}>
                <TextInput
                  style={{height: 40, flex: 17, margin: 25, width: '90%'}}
                  placeholder="enter keys"
                  onChangeText={(text) => this.setState({text})}
                  onSubmitEditing={() => this.sendKeys()}
                  value={this.state.text}/>
              </View>
              <View style={styles.keyblockrow}>
                <View style={styles.keyblock}>
                  <View style={styles.keyblockrow}>
                <Key chan={ch} sym="super+shift+f" icon="maximize"/>
                    <Key chan={ch} sym="XF86AudioPlay" icon="play"/>
                    <Key chan={ch} sym="super+w" icon="x"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Key chan={ch} sym="rwd"  icon="chevron-left"/>
                    <Key chan={ch} sym="play" icon="code"/>
                    <Key chan={ch} sym="fwd"  icon="chevron-right"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Key chan={ch} sym="quieter" icon="volume-1"/>
                    <Key chan={ch} sym="XF86AudioMute" icon="volume-x"/>
                    <Key chan={ch} sym="louder" icon="volume-2"/>
                  </View>
                </View>
                <View style={styles.keyblock}>
                  <View style={styles.keyblockrow}>
                    <Mouse chan={mc} sym="NW" icon="arrow-up-left"/>
                    <Mouse chan={mc} sym="N" icon="arrow-up"/>
                    <Mouse chan={mc} sym="NE" icon="arrow-up-right"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Mouse chan={mc} sym="W" icon="arrow-left"/>
                    <Mouse chan={mc} sym="click" icon="circle"/>
                    <Mouse chan={mc} sym="E" icon="arrow-right"/>
                  </View>
                  <View style={styles.keyblockrow}>
                    <Mouse chan={mc} sym="SW" icon="arrow-down-left"/>
                    <Mouse chan={mc} sym="S" icon="arrow-down"/>
                    <Mouse chan={mc} sym="SE" icon="arrow-down-right"/>
                  </View>
                </View>
              </View>
              <View style={styles.keyblockrow}>
                <Key chan={ch} sym="super+shift+e" icon="file-text"/>
                <Key chan={ch} sym="super+shift+w" icon="film"/>
                <Key chan={ch} sym="super+Tab" icon="external-link"/>
                <Key chan={ch} sym="sleep" icon="power"/>
              </View>
            </View>
        );
    }
}
