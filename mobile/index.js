/**
 * Sample React Native App
 * https://github.com/facebook/react-native
 * @flow
 */

import React, { Component } from 'react';
import {
    AppRegistry,
    Text,
    View
} from 'react-native';


import BrainzoRemote from './app/BrainzoRemote';

AppRegistry.registerComponent('Brainzo', () => BrainzoRemote);
