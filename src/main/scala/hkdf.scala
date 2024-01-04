/*
package bls

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import math.ceil
import com.google.protobuf.ByteString
import javax.xml.bind.DatatypeConverter
import java.math.BigInteger

class hkdf:

  val BLOCK_SIZE = 32
  
  def extract(salt: Array[Byte], ikm: Array[Byte]): Array[Byte] =
      val mac = Mac.getInstance("HmacSHA256")
      val secret = new SecretKeySpec(salt, "HmacSHA256")
      mac.init(secret)
      mac.doFinal(ikm)
  
  
  def expand(L: Int, prk: Array[Byte], info: Array[Byte]): Array[Byte] =
    val BLOCK_SIZE = 32
    val N = ceil(L.toDouble / BLOCK_SIZE).toInt
    var bytesWritten = 0
    var okm = Array[Byte]()
    var T = Array[Byte]()
  
    for i <- 1 to N
    do
      val h: Mac = if i == 1 then
        val infoPlusOne = info ++ Array[Byte](1.toByte)
        val key = new SecretKeySpec(prk, "HmacSHA256")
        val mac = Mac.getInstance("HmacSHA256")
        mac.init(key)
        mac.update(infoPlusOne)
        mac
      else
        val infoPlusI = info ++ Array[Byte](i.toByte)
        val key = new SecretKeySpec(prk, "HmacSHA256")
        val mac = Mac.getInstance("HmacSHA256")
        mac.init(key)
        mac.update(T)
        mac.update(infoPlusI)
        mac
      T = h.doFinal()
      var toWrite = L - bytesWritten
      if toWrite > BLOCK_SIZE then
        toWrite = BLOCK_SIZE
      okm = okm ++ T.slice(0, toWrite)
      bytesWritten += toWrite
    okm
  
  
  
  def extract_expand(L: Int, key: Array[Byte], salt: Array[Byte], info: Array[Byte]): Array[Byte] =
      val prk = extract(salt, key)
      expand(L, prk, info)

*/
