export default class Brainzo {
    static home = "http://brainzo.service.consul:4242";

    key(sym) {
        fetch(Brainzo.home + "/key/" + sym)
            .catch((error) => {
                console.error(error);
            });
    }

    mouse(sym) {
        fetch(Brainzo.home + "/mouse/" + sym)
            .catch((error) => {
                console.error(error);
            });
    }

    nowPlaying() {
        console.debug("get now playing")
        return fetch(Brainzo.home + "/np/get")
            .catch((error) => {
                console.warn(error);
            });
    }
}
