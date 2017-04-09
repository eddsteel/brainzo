import {chan, go, take, timeout} from 'js-csp';

export default class Receiver {
    constructor(ch, f) {
        go(function* () {
            while(true) {
              var key = yield take(ch)
              console.debug("I got", key);
                yield f(key);
                yield timeout(50); // give the api a chance to do something
            }
        });
    }
};
