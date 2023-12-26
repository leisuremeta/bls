package bls
/*
object bls12381:
    val x = BigInt("-0xD201000000010000")

    val q = BigInt("0x1A0111EA397FE69A4B1BA7B6434BACD764774B84F38512BF6730D2A0F6B0F6241EABFFFEB153FFFFB9FEFFFFFFFFAAAB")


    val a = Fq(q: Int, 0)
    val b = Fq(q: Int, 4)

    a_twist = Fq2(q: Int, 0, 0)
    b_twist = Fq2(q: Int, 4, 4)

    val gx = Fq(
        q: Int,
        BigInt("0x17F1D3A73197D7942695638C4FA9AC0FC3688C4F9774B905A14E3A3F171BAC586C55E83FF97A1AEFFB3AF00ADB22C6BB"),
    )
    val gy = Fq(
        q: Int,
        BigInt("0x08B3F481E3AAA0F1A09E30ED741D8AE4FCF5E095D5D00AF600DB18CB2C04B3EDD03CC744A2888AE40CAA232946C5E7E1"),
    )

    val g2x = Fq2(
        q: Int,
        BigInt("352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160"),
        BigInt("3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758"),
    )
    val g2y = Fq2(
        q: Int,
        BigInt("1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905"),
        BigInt("927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582"),
    )

    """The order of all three groups (g1, g2, and gt). Note, the elliptic curve E_twist
    actually has more valid points than this. This is relevant when hashing onto the
    curve, where we use a point that is not in g2, and map it into g2."""

    val n = BigInt("0x73EDA753299D7D483339D80809A1D80553BDA402FFFE5BFEFFFFFFFF00000001")

    //Cofactor used to generate r torsion points
    val h = BigInt("0x396C8C005555E1568C00AAAB0000AAAB")

    //https://tools.ietf.org/html/draft-irtf-cfrg-hash-to-curve-07#section-8.8.2
    val h_eff = BigInt("0xBC69F08F2EE75B3584C6A0EA91B352888E2A8E9145AD7689986FF031508FFE1329C2F178731DB956D82BF015D1212B02EC0EC69D7477C1AE954CBC06689F6A359894C0ADEBBF6B4E8020005AAA95551")

    //Embedding degree
    val k = 12

    //sqrt(-3) mod q))
    val sqrt_n3 = BigInt("1586958781458431025242759403266842894121773480562120986020912974854563298150952611241517463240701")

    //(sqrt(-3) - 1) / 2 mod q
    val sqrt_n3m1o2 = BigInt("793479390729215512621379701633421447060886740281060493010456487427281649075476305620758731620350")

    //This is the normal elliptic curve. G1 points are on here.
    def parameters() = (q, a, b, gx, gy, g2x, g2y, n, h, x, k, sqrt_n3, sqrt_n3m1o2)


    """This is the sextic twist used to send elements of G2 from
    coordinates in Fq12 to coordinates in Fq2. It's isomorphic
    to the above elliptic curve. See Page 63 of Costello."""
    def parameters_twist() = (
            q,
            a_twist,
            b_twist,
            gx,
            gy,
            g2x,
            g2y,
            n,
            h_eff,
            x,
            k,
            sqrt_n3,
            sqrt_n3m1o2,
        )
    
*/
