callback Qux = void (short x);

[Constructor]
interface Bar {};

interface Foo {
    ///- requires bar != null
    void use_bar(Bar? bar);

    ///- ensures ret > 0.0
    float pos();

    ///- callbacks qux when (i > 0) with (-10)
    short async(short i, Qux qux);

    ///- requires (if bos.Bar? then (bos.Bar != null) else (bos.Int > 0))
    void use_union((Bar or short) bos);
};
