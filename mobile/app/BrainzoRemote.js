import React, { Component } from 'react';
import {
    View,
    Text,
    StyleSheet
} from 'react-native';
import { chan } from 'js-csp';

import Brainzo from './util/Brainzo';
import Receiver from './util/Receiver';
import Keyboard from './components/Keyboard';
import NowPlaying from './components/NowPlaying';

export default class BrainzoRemote extends Component {
    constructor(props) {
        super(props);
        var ch = chan(30);
        var bz = new Brainzo();
        var recv = new Receiver(ch, (k) => bz.key(k));
        this.state = {
            'brainzo': bz,
            'keyChan': ch,
            'receiver': recv,
        };
    }

    render() {
        return (
            <View style={styles.container}>
              <NowPlaying brainzo={this.state.brainzo}/>
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

});
