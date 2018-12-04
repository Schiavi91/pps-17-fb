package communication

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import com.spingo.op_rabbit.{RabbitMarshaller, RabbitUnmarshaller}

/** Provides features of Marshaller and Unmarshaller to messages through the use of serialization. */
object MessageFormat {

  type MyFormat[A] = RabbitMarshaller[A] with RabbitUnmarshaller[A]

  /** Provides Marshaller and Unmarshaller to messages. */
  def format[A]: MyFormat[A] = new RabbitMarshaller[A] with RabbitUnmarshaller[A] {
    val contentType = "text/plain"
    protected val contentEncoding = Some("UTF-8")
    def marshall(value: A): Array[Byte] = serialize(value)

    def unmarshall(value: Array[Byte], contentType: Option[String], charset: Option[String]): A =
      deserialize(value).asInstanceOf[A]
  }

  private def serialize(value: Any): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close()
    stream.toByteArray
  }

  private def deserialize(bytes: Array[Byte]): Any = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close()
    value
  }
}
