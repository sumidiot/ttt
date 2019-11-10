package sumidiot.bom.ttt

import cats.effect._
import doobie._
import doobie.implicits._
import doobie.h2._
import doobie.util.ExecutionContexts

import sumidiot.bom.ttt.Common._

/**
 * This object provides some common elements around a database-backed TicTacToe
 * implementation. It is intended to be used in Final and OO style implementations.
 */
object Doobie {

  // The next two comments are copied directly from the doobie tutorial
  // We need a ContextShift[IO] before we can construct a Transactor[IO]. The passed ExecutionContext
  // is where nonblocking operations will be executed. For testing here we're using a synchronous EC.
  implicit val cs = IO.contextShift(ExecutionContexts.synchronous)

  // this block is copied from the h2 doobie example
  val transactor: Resource[IO, H2Transactor[IO]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](2) // our connect EC
      be <- Blocker[IO]                              // our blocking EC
      xa <- H2Transactor.newH2Transactor[IO](
              "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1",  // connect URL
              "sa",                                  // username
              "",                                    // password
              ce,                                    // await connection here
              be                                     // execute JDBC operations here
            )
    } yield xa

  def run[A](cio: ConnectionIO[A]): IO[A] =
    transactor.use { xa => cio.transact(xa) }

  /**
   * Hack to spin up the in-memory database
   */
  def initializeDB(): Unit = {
    run(sql"""CREATE TABLE turn (ps VARCHAR)""".update.run).unsafeRunSync
    run(sql"""CREATE TABLE bps (rs VARCHAR, cs VARCHAR, ps VARCHAR)""".update.run).unsafeRunSync
    run(sql"""INSERT INTO turn (ps) VALUES ('X')""".update.run).unsafeRunSync
  }

  /**
   * The next couple case classes and utility methods are things we'd imagine replacing with
   * cleaner doobie-based interaction with the database, but this works for now.
   */

  case class Turn(ps: String)
  def player(t: Turn): Player =
    Player(t.ps(0))
      
  case class BoardPositionState(rs: String, cs: String, ps: String)
  def position(bps: BoardPositionState): Position =
    Position(s"${bps.rs}${bps.cs}").get // sinner
  def player(bps: BoardPositionState): Player =
    Player(bps.ps(0))


  /**
   * These queries represent, generally, the utility methods one might use to interact with
   * a database in a way that would support the operations we're expecting for our TicTacToe-s.
   * In reality, all these methods would have better error checking and such, but in reality
   * we probably wouldn't be doing any of this for TicTacToe.
   */
  object Queries {

    // get the current player owning a position
    def info(p: Position): ConnectionIO[Option[Player]] =
      sql"select rs, cs, ps from bps where rs = ${p.row.toString} and cs = ${p.col.toString}"
        .query[BoardPositionState]  // Query0[BoardPositionState]
        .option                     // ConnectionIO[Option[BoardPositionState]]
        .map(_.map(player))         // ConnectionIO[Option[Player]]

    // this is the `forceTake` interpretation of Final
    def take(pos: Position): ConnectionIO[Unit] =
      for {
        curPlayer <- turn
        _ <- sql"delete from bps where rs = ${pos.row.toString} and cs = ${pos.col.toString}".update.run
        _ <- sql"insert into bps (rs, cs, ps) values (${pos.row.toString}, ${pos.col.toString}, ${curPlayer.toString})".update.run
      } yield { () }

    def turn(): ConnectionIO[Player] =
      sql"select ps from turn"
        .query[Turn]  // Query0[Turn]
        .unique       // ConnectionIO[Turn]
        .map(player)  // ConnectionIO[Player]

    def switchPlayer(): ConnectionIO[Unit] =
      for {
        curPlayer <- turn
        nextPlayer = Player.other(curPlayer)
        _ <- sql"update turn set ps = ${nextPlayer.toString}".update.run
      } yield { () }

  }

}
