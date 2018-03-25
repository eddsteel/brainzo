export default class Brainzo {
    static home = "http://brainzo.service.consul:4242";

    key(sym) {
        fetch(Brainzo.home + "/key/" + sym)
            .catch((error) => {
                console.error(error);
            });
    }
}
