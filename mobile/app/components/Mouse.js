import React, { Component } from 'react';
import {
    View
} from 'react-native';
import Icon from 'react-native-vector-icons/Feather';
import Brainzo from '../util/Brainzo'
import { putAsync } from 'js-csp';

export default class Mouse extends React.Component {
    constructor(props) {
        super(props);

        this.sendMouse = (chan, sym) => {
            console.debug("sending ", sym);
            putAsync(chan, sym);
        }
    }

    render() {
        return (
            <View style={{margin: 5}}>
              <Icon.Button style={{alignItems: 'center'}}
                size={16}
                onPress={() => this.sendMouse(this.props.chan, this.props.sym)}
                name={this.props.icon}/>
            </View>
        );
    }
}
