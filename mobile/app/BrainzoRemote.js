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
import Keyboard from './components/Keyboard';

export default class BrainzoRemote extends Component {
    constructor(props) {
        super(props);
        var ch = chan(30);
        var bz = new Brainzo();
        var recv = new Receiver(ch, (k) => bz.key(k));
        this.state = {
            'keyChan': ch,
            'receiver': recv,
        };
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

              <Keyboard chan={this.state.keyChan} />
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

});
