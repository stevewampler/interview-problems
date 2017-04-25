package com.sgw.typeclasses

import scala.annotation.implicitNotFound

case class AccountSet(id: Long)

case class User(id: Long)

//trait Entity

// the type class
@implicitNotFound("No member of type class AccountSets[T] in scope for T = ${T}")
trait AccountSets[T] {
  def accountSets(entity: T): Option[List[AccountSet]]
}

@implicitNotFound("No member of type class HasModifiedUsers[T] in scope for T = ${T}")
trait HasModifiedUsers[T] {
  def modifiedBy(entity: T): Option[List[User]]
}

object Message {
  implicit object AccountSetsMessage extends AccountSets[Message] {
    def accountSets(message: Message): Option[List[AccountSet]] = Some(message.accountSets)
  }

  implicit object HasModifiedUsersMessage extends HasModifiedUsers[Message] {
    def modifiedBy(message: Message): Option[List[User]] = Some(message.modifiedBy)
  }
}

case class Message(id: Long, accountSets: List[AccountSet], modifiedBy: List[User]) // extends Entity

object Image {
  implicit object HasModifiedUsersImage extends HasModifiedUsers[Image] {
    def modifiedBy(image: Image): Option[List[User]] = Some(image.modifiedBy)
  }
}

case class Image(id: Long, modifiedBy: List[User]) // extends Entity


trait Predicate[T] {
//  import Approvals.{HasAccountSets, HasModifiedUsers}

  def apply(entity: T): Boolean

  def toDSL: String
}

//import Message.AccountSets

case class HasAccountSet[T : AccountSets](accountSet: AccountSet) extends Predicate[T] {

  def apply(entity: T): Boolean = implicitly[AccountSets[T]].accountSets(entity).exists(_.contains(accountSet))

  def toDSL: String = s"HasAccountSet ${accountSet.id}"
}

//object WasModifiedBy {
//  import Approvals.HasModifiedUsers
//
//  def apply[T : HasModifiedUsers](entity: T): Boolean = implicitly[HasModifiedUsers[T]].modifiedBy(entity).exists(_.contains(user))
//}
//import Approvals.HasModifiedUsers

case class WasModifiedBy[T : HasModifiedUsers](user: User) extends Predicate[T] {

  def apply(entity: T): Boolean = implicitly[HasModifiedUsers[T]].modifiedBy(entity).exists(_.contains(user))

  def toDSL: String = s"WasModifiedBy ${user.id}"
}

object PredicateMessageReads {
  def reads(dsl: String): Option[Predicate[Message]] = {
    dsl.split(' ') match {
      case Array("HasAccountSet", value) => Some(HasAccountSet(AccountSet(value.toInt)))
      case Array("WasModifiedBy", value) => Some(WasModifiedBy(User(value.toInt)))
      case _ => None
    }
  }
}

object PredicateImageReads {
  def reads(dsl: String): Option[Predicate[Image]] = {
    dsl.split(' ') match {
      case Array("WasModifiedBy", value) => Some(WasModifiedBy(User(value.toInt)))
      case _ => None
    }
  }
}

case class ApproverConfig[T](
  shouldApprovePredicate: Predicate[T],
  shouldReapprovePredicate: Predicate[T]
)

object TypeClassTest {
  def main(args: Array[String]): Unit = {
    val user1 = User(1)
    val user2 = User(2)

    val accountSet1 = AccountSet(1)
    val accountSet2 = AccountSet(2)

    val message1 = Message(id = 1, accountSets = List(accountSet1), modifiedBy = List(user1))
    val message2 = Message(id = 2, accountSets = List(accountSet1, accountSet2), modifiedBy = List(user1, user2))

    val hasAccountSet1: Predicate[Message] = HasAccountSet(accountSet1)
    val hasAccountSet2: Predicate[Message] = HasAccountSet(accountSet2)

    assert(hasAccountSet1(message1))
    assert(!hasAccountSet2(message1))
    assert(hasAccountSet1(message2))
    assert(hasAccountSet2(message2))

    val wasModifiedBy1: Predicate[Message] = WasModifiedBy(user1)
    val wasModifiedBy2: Predicate[Message] = WasModifiedBy(user2)

    assert(wasModifiedBy1(message1))
    assert(!wasModifiedBy2(message1))
    assert(wasModifiedBy1(message2))
    assert(wasModifiedBy2(message2))

    val hasAccountSet1DSL = hasAccountSet1.toDSL
    val hasAccountSet2DSL = hasAccountSet2.toDSL

    assert(hasAccountSet1DSL == "HasAccountSet 1")
    assert(hasAccountSet2DSL == "HasAccountSet 2")

    val wasModifiedBy1DSL = wasModifiedBy1.toDSL
    val wasModifiedBy2DSL = wasModifiedBy2.toDSL

    assert(wasModifiedBy1DSL == "WasModifiedBy 1")
    assert(wasModifiedBy2DSL == "WasModifiedBy 2")

    assert(PredicateMessageReads.reads(hasAccountSet1DSL).nonEmpty)
    PredicateMessageReads.reads(hasAccountSet1DSL).foreach(predicate => predicate == hasAccountSet1)

    assert(PredicateImageReads.reads(hasAccountSet1DSL).isEmpty)
//    PredicateMessageReads.reads(hasAccountSet1DSL).foreach(predicate => predicate == hasAccountSet1)

//    assert(fromDSL(wasModifiedBy1DSL).nonEmpty)
//    fromDSL(wasModifiedBy1DSL).foreach(predicate => predicate == wasModifiedBy1)

    val approverConfig = ApproverConfig[Message](
      shouldApprovePredicate = hasAccountSet1,
      shouldReapprovePredicate = wasModifiedBy1
    )

    assert(approverConfig.shouldApprovePredicate(message1))
  }
}