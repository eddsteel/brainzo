import React, { Component } from 'react';
import {
    View
} from 'react-native';
import Icon from 'react-native-vector-icons/MaterialIcons';
import Brainzo from '../util/Brainzo'
import { putAsync } from 'js-csp';

export default class Key extends React.Component {
    constructor(props) {
        super(props);

        this.sendKey = (chan, sym) => {
            console.debug("sending ", sym);
            putAsync(chan, sym);
        }
    }

    render() {
        var sym = this.props.sym;
        var chan = this.props.chan;
        return (
            <View style={{margin: 5}}>
              <Icon.Button style={{alignItems: 'center'}}
                size={18}
                onPress={() => this.sendKey(chan, sym)}
                name={this.props.icon}/>
            </View>
        );
    }
}
