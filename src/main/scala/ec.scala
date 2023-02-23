package bls
import scala.math._

case class EC(q: BigInt, a: BigInt, b: BigInt, gx: BigInt, gy: BigInt, 
                g2x: BigInt, g2y: BigInt, n: BigInt, h: BigInt, x: BigInt, 
                k: BigInt, sqrt_n3: BigInt, sqrt_n3m1o2: BigInt)

object AffinePoint:
    val default_ec = EC(bls12381.parameters())
    val default_ec_twist = EC(bls12381.parameters_twist())
end AffinePoint

class AffinePoint[T <: FieldElement[T]](x: T, y: T, infinity: Boolean, ec: EC = default_ec):
    require(x.getClass() == y.getClass(), "x,y should be field elements")
    val FE = x.getClass()
    val _x = x
    val _y = y
    val _infinity = infinity
    val _ec = ec

    def isOnCurve(): Boolean =
        if _infinity then
            true
        val left = _y * _y
        val right = _x * _x * _x + _ec.a * _x + _ec.b
        left == right

    def +(other: AffinePoint[T]): AffinePoint[T] =
        if other == 0 then
            this
        add_points(this, other, _ec, FE)

    def radd(other: AffinePoint[T]): AffinePoint[T] =
        this + other

    def -(other: AffinePoint[T]): AffinePoint[T] =
        this + other.negate()

    def negate(): AffinePoint[T] =
        new AffinePoint(_x, -_y, _infinity, _ec)

    override def toString: String =
        "AffinePoint(x=" + _x + ", y=" + _y + ", i=" + _infinity + ")\n"
        
    def toBytes: Array[Byte] =
        point_to_bytes(this, _ec, FE)

    override def ==(other: Any): Boolean =
        other match
            case that: AffinePoint[T] =>
                (_x == that._x) && (_y == that._y) && (_infinity == that._infinity)
            case _ => false
    def *(c: T): AffinePoint[T] =
        scalar_mult_jacobian(c, this.to_jacobian(), _ec).to_affine()

    def rmul(c: Fq): AffinePoint[T] =
        this.*(c)

    def to_jacobian(): JacobianPoint[T] =
        new JacobianPoint(_x, _y, FE.one(_ec.q), _infinity, _ec)

    def copy(): AffinePoint[T] =
        new AffinePoint(_x.copy(), _y.copy(), _infinity, _ec)

class JacobianPoint(x: Fq, y: Fq, z: Fq, infinity: Boolean, ec: default_ec):

    if
        !(x.isInstanceOf[Fq] || x.isInstanceOf[FieldExtBase])
        || !(y.isinstanceOf[Fq] || y.isinstanceOf[FieldExtBase])
        || !(z.isinstanceOf[Fq] || z.isinstanceOf[FieldExtBase])
    then
        throw new Exception("x, y, z should be field elements")

    val FE = x.getClass
    val this.x = x
    val this.y = y
    val this.z = z
    val this.infinity = infinity
    val this.ec = ec

    def isOnCurve(): Boolean =
        if this.infinity then
            true
        this.toAffine().isOnCurve()

    def negate(): JacobianPoint =
        this.toAffine().negate().toJacobian()

    def toAffine(): AffinePoint =
        if this.infinity then
            new AffinePoint(
                Fq.zero(this.ec.q), Fq.zero(this.ec.q), this.infinity, this.ec
            )
        val new_x = this.x / (pow(this.z, 2))
        val new_y = this.y / (pow(this.z, 3))
        new AffinePoint(new_x, new_y, this.infinity, this.ec)

    def checkValid(): Unit =
        require(isOnCurve())
        require(this * ec.n == G2Infinity())

    def getFingerprint(): Int =
        val ser = bytes(this)
        int.fromBytes(hash256(ser).take(4), "big")

    def +(other: JacobianPoint): JacobianPoint =
        if other == 0 then
            this
        else if !other.isInstanceOf[JacobianPoint] then
            throw new Exception("Incorrect object")
        else
            addPointsJacobian(this, other, ec, FE)

    def radd(other: JacobianPoint): JacobianPoint =
        this.+(other)

    def ==(other: JacobianPoint): Boolean =
        if !other.isInstanceOf[JacobianPoint] then
            false
        else
            toAffine() == other.toAffine()

    def !=(other: JacobianPoint): Boolean =
        !this.==(other)

    def *(c: Any): JacobianPoint =
        if !c.isInstanceOf[Int] && !c.isInstanceOf[Fq] then
            throw new Exception("Error, must be int or Fq")
        scalarMultJacobian(c, this, ec)

    def rmul(c: Any): JacobianPoint =
        this.*(c)

    def !(): JacobianPoint =
        toAffine().negate().toJacobian()

    override def toString(): String =
        s"JacobianPoint(x=${x.toString()}, y=${y.toString()}z=${z.toString()}, i=${infinity.toString()})\n"

    def toBytes(): Array[Byte] =
        pointToBytes(this, ec, FE)

    def copy(): JacobianPoint =
        new JacobianPoint(x.copy(), y.copy(), z.copy(), infinity, ec)

    override def hashCode(): Int =
        int.fromBytes(toBytes(), "big")

def sign_Fq(element: Fq, ec: EC = default_ec): Boolean =
    element > Fq(ec.q, ((ec.q - 1) / 2))

def sign_Fq2(element: (Fq, Fq), ec: EC_Twist = default_ec_twist): Boolean =
    if element._2 == Fq(ec.q, 0) then
        sign_Fq(element.head)
    else
        element._2 > Fq(ec.q, ((ec.q - 1) / 2))


def point_to_bytes(point: Point, ec: EC, FE: Fq): Array[Byte] =
    if point.isInstanceOf[JacobianPoint] then
        point = point.asInstanceOf[JacobianPoint].toAffine()
    if !point.isInstanceOf[AffinePoint] then
        throw new Exception("point should either be JacobianPoint or AffinePoint")
    val output = point.x.bytes

    if point.infinity then
        Array[Byte](0x40) ++ Array.fill(output.length - 1)(0x00)

    val sign = if (FE == classOf[Fq]) sign_Fq(point.y, ec) else sign_Fq2(point.y, ec)

    if sign then
        output(0) = (output(0) | 0xA0).toByte
    else
        output(0) = (output(0) | 0x80).toByte
    output

def _to_point(buffer: Array[Byte], ec: EC, FE: FE): Jacobian =
    if FE == Fq then
        if buffer.length != 48 then
            throw new Exception("G1Elements must be 48 bytes")
    else if FE == Fq2 then
        if buffer.length != 96 then
            throw new Exception("G2Elements must be 96 bytes")
    else
        throw new Exception("Invalid FE")

    m_byte = buffer(0) & 0xE0

    if m_byte == 0x20 || m_byte == 0x60 || m_byte == 0xE0 then
        throw new Exception("Invalid first three bits")

    C_bit = (m_byte & 0x80) >> 7 
    I_bit = (m_byte & 0x40) >> 6 
    S_bit = (m_byte & 0x20) >> 5 

    if C_bit == 0 then
        throw new Exception("First bit must be 1 (only compressed points)")

    buffer(0) = (buffer(0) & 0x1F).toByte
    if I_bit == 1 then
        if buffer.exists(_ != 0) then
            throw new Exception("Point at infinity set, but data not all zeroes")
        AffinePoint(FE.zero(ec.q), FE.zero(ec.q), true, ec).to_jacobian()

    x = FE.from_bytes(buffer, ec.q)
    y_value = y_for_x(x, ec, FE)

    val sign_fn = 
        if FE == Fq then
            sign_Fq 
        else 
            sign_Fq2

    if sign_fn(y_value, ec) == S_bit then
        y = y_value
    else
        y = -y_value
    AffinePoint(x, y, false, ec).to_jacobian()

def y_for_x(x: FE, ec: EC = default_ec, FE: FE = Fq): AffinePoint =
    val u = x * x * x + ec.a * x + ec.b
    val y = u.modsqrt()
    if y == 0 || !AffinePoint(x, y, false, ec).isOnCurve() then
        throw new Exception("No y for point x")
    y

def double_point(p1: AffinePoint, ec: EC = default_ec, FE: FE = Fq): AffinePoint =
    val x = p1.x
    val y = p1.y
    val left = (3 * x * x) + ec.a
    val s = left / (2 * y)
    val new_x = s * s - x - x
    val new_y = s * (x - new_x) - y
    AffinePoint(new_x, new_y, false, ec)

def add_points(p1: AffinePoint, p2: AffinePoint, ec: EC = default_ec, FE: FE = Fq): AffinePoint =
    require(p1.isOnCurve())
    require(p2.isOnCurve())
    if p1.infinity then
        p2
    if p2.infinity then
        p1
    if p1 == p2 then
        double_point(p1, ec, FE)
    if p1.x == p2.x then
        AffinePoint(FE.zero(ec.q), FE.zero(ec.q), true, ec)
    val x1 = p1.x
    val y1 = p1.y
    val x2 = p2.x
    val y2 = p2.y
    val s = (y2 - y1) / (x2 - x1)
    val new_x = s * s - x1 - x2
    val new_y = s * (x1 - new_x) - y1
    AffinePoint(new_x, new_y, false, ec)

def double_point_jacobian(p1: JacobianPoint, ec: EC = default_ec, FE: Fq = Fq): JacobianPoint =
    val X = p1.x
    val Y = p1.y
    val Z = p1.z
    if Y == FE.zero(ec.q) || p1.infinity then
        new JacobianPoint(FE.one(ec.q), FE.one(ec.q), FE.zero(ec.q), true, ec)
    val S = Fq(ec.q, 4) * X * Y * Y
    val Z_sq = Z * Z
    val Z_4th = Z_sq * Z_sq
    val Y_sq = Y * Y
    val Y_4th = Y_sq * Y_sq
    val M = Fq(ec.q, 3) * X * X
    M += ec.a * Z_4th
    val X_p = M * M - Fq(ec.q, 2) * S
    val Y_p = M * (S - X_p) - Fq(ec.q, 8) * Y_4th
    val Z_p = Fq(ec.q, 2) * Y * Z
    new JacobianPoint(X_p, Y_p, Z_p, false, ec)

def add_points_jacobian(p1: JacobianPoint, p2: JacobianPoint, ec: EC = default_ec, FE: Fq = Fq): JacobianPoint =
    if p1.infinity then
        p2
    if p2.infinity then
        p1
    var U1 = p1.x * (p2.z * p2.z)
    var U2 = p2.x * (p1.z * p1.z)
    var S1 = p1.y * (p2.z * p2.z * p2.z)
    var S2 = p2.y * (p1.z * p1.z * p1.z)
    if U1 == U2 then
        if S1 != S2 then
            new JacobianPoint(FE.one(ec.q), FE.one(ec.q), FE.zero(ec.q), true, ec)
        else
            double_point_jacobian(p1, ec, FE)
    val H = U2 - U1
    val R = S2 - S1
    val H_sq = H * H
    val H_cu = H * H_sq
    val X3 = R * R - H_cu - Fq(ec.q, 2) * U1 * H_sq
    val Y3 = R * (U1 * H_sq - X3) - S1 * H_cu
    val Z3 = H * p1.z * p2.z
    new JacobianPoint(X3, Y3, Z3, false, ec)

def scalarMult(c: Int, p1: AffinePoint, ec: EllipticCurve = defaultEC, FE: FiniteField = Fq): AffinePoint =
    if p1.infinity || c % ec.q == 0 then 
        AffinePoint(FE.zero(ec.q), FE.zero(ec.q), ec)
    var result = AffinePoint(FE.zero(ec.q), FE.zero(ec.q), true, ec)
    var addend = p1
    while c > 0 do
        if (c & 1) == 1 then
            result = result + addend
        addend = addend + addend
        c = c >> 1
    result

def scalarMultJacobian(c: Int, p1: JacobianPoint, ec: EllipticCurve = defaultEC, FE: FiniteField = Fq): JacobianPoint =
    if (p1.infinity || c % ec.q == 0) then
        JacobianPoint(FE.one(ec.q), FE.one(ec.q), FE.zero(ec.q), true, ec)

    var result = JacobianPoint(FE.one(ec.q), FE.one(ec.q), FE.zero(ec.q), true, ec)
    var addend = p1
    while c > 0 do
        if (c & 1) == 1 then
            result = result + addend
        addend = addend + addend
        c = c >> 1
    result

def G1Generator(ec: EllipticCurve = defaultEC): JacobianPoint =
    AffinePoint(ec.gx, ec.gy, false, ec).toJacobian()

def G2Generator(ec: EllipticCurve = defaultECTwist): JacobianPoint =
    AffinePoint(ec.g2x, ec.g2y, false, ec).toJacobian()

def G1Infinity(ec: EllipticCurve = defaultEC, FE: FiniteField = Fq): JacobianPoint =
    JacobianPoint(FE.one(ec.q), FE.one(ec.q), FE.zero(ec.q), true, ec)

def G2Infinity(ec: EllipticCurve = defaultECTwist, FE: FiniteField = Fq2): JacobianPoint =
    JacobianPoint(FE.one(ec.q), FE.one(ec.q), FE.zero(ec.q), true, ec)


def G1FromBytes(buffer: Array[Byte], ec: EllipticCurve = defaultEC, FE: FiniteField = Fq): JacobianPoint =
    bytesToPoint(buffer, ec, FE)

def G2FromBytes(buffer: Array[Byte], ec: EllipticCurve = defaultECTwist, FE: FiniteField = Fq2): JacobianPoint =
    bytesToPoint(buffer, ec, FE)

def untwist(point: AffinePoint, ec: EllipticCurve = defaultEC): AffinePoint =
    val f = Fq12.one(ec.q)
    val wsq = Fq12(ec.q, f.root, Fq6.zero(ec.q))
    val wcu = Fq12(ec.q, Fq6.zero(ec.q), f.root)
    AffinePoint(point.x / wsq, point.y / wcu, false, ec)

def twist(point: AffinePoint, ec: EllipticCurve = defaultECTwist): AffinePoint =
    val f = Fq12.one(ec.q)
    val wsq = Fq12(ec.q, f.root, Fq6.zero(ec.q))
    val wcu = Fq12(ec.q, Fq6.zero(ec.q), f.root)
    val newX = point.x * wsq
    val newY = point.y * wcu
    AffinePoint(newX, newY, false, ec)

def evalIso(P: JacobianPoint, mapCoeffs: List[List[Fq2]], ec: EllipticCurve): JacobianPoint = 
    val (x, y, z) = (P.x, P.y, P.z)
    val mapVals = Array.fill[Option[Fq2]](4)(None)
    val maxOrd = mapCoeffs.map(_.length).max
    val zPows = Array.fill[Option[Fq2]](maxOrd)(None)
    zPows(0) = z ** 0
    zPows(1) = z ** 2
    for idx <- 2 until zPows.length
    do
        assert(zPows(idx - 1).isDefined)
        assert(zPows(1).isDefined)
        zPows(idx) = zPows(idx - 1).get * zPows(1).get
    for (idx, coeffs) <- mapCoeffs.zipWithIndex
    do
        val coeffsZ = (reversed(coeffs) zip zPows.take(coeffs.length)).map { case (c, zpow) => zpow * c }
        var tmp = coeffsZ(0)
        for coeff <- coeffsZ.drop(1)
        do
            tmp *= x
            tmp += coeff
        mapVals(idx) = tmp
    assert(mapCoeffs(1).length + 1 == mapCoeffs(0).length)
    assert(zPows(1).isDefined)
    assert(mapVals(1).isDefined)
    mapVals(1) = mapVals(1).get * zPows(1).get
    assert(mapVals(2).isDefined)
    assert(mapVals(3).isDefined)
    mapVals(2) = mapVals(2).get * y
    mapVals(3) = mapVals(3).get * z ** 3

    val Z = mapVals(1).get * mapVals(3).get
    val X = mapVals(0).get * mapVals(3).get * Z
    val Y = mapVals(2).get * mapVals(1).get * Z * Z
    new JacobianPoint(X, Y, Z, P.infinity, ec)