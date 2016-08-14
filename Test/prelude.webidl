callback Qux = void (short x);

dictionary A {
  long c;
  long g;
};

[HTMLConstructor]
interface HTMLBar {};

[
///- ensures (ret != null)
Constructor
]
interface Bar {
    attribute short myAttr;
};

interface Foo {
    const short SOME_CONST = 0x00000100;

    ///- requires bar != null
    void use_bar(Bar? bar);

    ///- ensures ret > 0.0
    float pos();

    ///- callbacks qux when (i > 0) with (-10)
    short async(short i, Qux qux);

    ///- requires (if bos.Bar? then (bos.Bar != null) else (bos.Int > 0))
    void use_union((Bar or short) bos);

    ///- requires (a != null && a.c > 10)
    void use_dict(A a);
};
