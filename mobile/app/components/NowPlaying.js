import React, { PureComponent } from 'react';
import {
    StyleSheet,
    View
} from 'react-native';
import TextTicker from 'react-native-text-ticker'
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
              <TextTicker
                style={{
                    fontSize: 20,
                    textAlign: 'center',
                    margin: 10,
                    color: '#ff00dd',

                }}
                duration={15000}
                loop
                bounce>
                {np}
                </TextTicker>
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
