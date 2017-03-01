export default class Brainzo {
    static home = "http://192.168.1.5:4242";

    key(sym) {
        fetch(Brainzo.home + "/key/" + sym)
            .catch((error) => {
                console.error(error);
            });
    }
}
