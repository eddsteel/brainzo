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
        var ch1 = chan(10);
        var ch2 = chan(10);
        var ch3 = chan(10);
        var bz = new Brainzo();
        var recv = new Receiver(ch1, (k) => bz.key(k));
        var msrcv = new Receiver(ch2, (m) => bz.mouse(m));
        var mprcv = new Receiver(ch3, (m) => bz.mpris(m));
        this.state = {
            'brainzo': bz,
            'keyChan': ch1,
            'mouseChan': ch2,
            'mprisChan': ch3
        };
    }

    render() {
        return (
            <View style={styles.container}>
              <NowPlaying brainzo={this.state.brainzo}/>
                <Keyboard chan={this.state.keyChan}
                          mouseChan={this.state.mouseChan}
                          mprisChan={this.state.mprisChan}
                />
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
