import React, { PureComponent } from 'react';
import {
    StyleSheet,
    Text,
    View
} from 'react-native';
import Brainzo from '../util/Brainzo'

export default class NowPlaying extends PureComponent {
    constructor(props) {
        super(props);
        this.state = {
            nowPlaying: "loading…",
            loading: true
        }
        this.bz = props.brainzo;
    }

    render() {
        var np = this.state.nowPlaying;
        console.log("now playing " + np)
        return (
            <Text
                style={{
                    fontSize: 20,
                    textAlign: 'center',
                    margin: 5,
                    color: '#ff00dd',

                }}>
                {np}
            </Text>
        );
    }

    update() {
        var nowPlaying = this.bz.nowPlaying();
        nowPlaying
            .then((resp) => resp.text())
            .then((np) => this.setState({nowPlaying: np, loading: false}));
    }

    componentDidMount() {
        this.update();
        setInterval(() => this.update(), 10000)
    }
}
