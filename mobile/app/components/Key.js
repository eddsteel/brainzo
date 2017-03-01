import React, { Component } from 'react';
import {
    View
} from 'react-native';
import Icon from 'react-native-vector-icons/MaterialIcons';
import Brainzo from '../util/Brainzo'

export default class Key extends React.Component {
    constructor(props) {
        super(props);

        this.hitKey = (sym) => {
            fetch(Brainzo.home + "/key/" + sym)
                .catch((error) => {
                    console.error(error);
                });
        }
    }

    render() {
        var sym = this.props.sym;
        return (
            <View style={{margin: 5}}>
              <Icon.Button style={{alignItems: 'center'}}
                size={18}
                onPress={() => this.hitKey(this.props.sym)}
                name={this.props.icon}/>
            </View>
        );
    }
}
