import React, { Component } from 'react';
import {
    View,
    Text
} from 'react-native';

export default class CoolText extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            //            counter: new Animated.Value(0),
            counter: 0,
        };
    }

    render() {
        var props = this.props;
        var state = this.state;
        var pstyl = this.props.style;
        var objs = props.letters.map(function(letr, i) {
            var cnt = (i + state.counter) % props.titleColors.length;
            var styl = {
                color: props.titleColors[cnt],
                transform: [],
            };
            return {letter: letr, style: [pstyl, styl]}
        });

        return (
            <View style={{flexDirection: 'row'}}>
              {objs.map(function(obj, i) {
                  return <Text style={obj.style} key={i} >
                      {obj.letter}
                  </Text>;
              })}
            </View>
        );
    }

    componentDidMount() {
        setInterval(() => {
            this.setState({counter: (this.state.counter + 1 % this.props.titleColors.length)});
        }, 300);
    }
}
