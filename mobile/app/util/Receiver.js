import {chan, go, take} from 'js-csp';

export default class Receiver {
    constructor(ch, f) {
        go(function* () {
            while(true) {
                var key = yield take(ch)
                console.debug("I got", key);
                f(key);
            }
        });
    }
};
