/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package annotation

import scala.annotation.Annotation
import scala.annotation.StaticAnnotation
import scala.annotation.meta._

/** The annotated symbols is not extracted at all. For internal usage only. */
class ignore extends Annotation


/** The annotated function or class' methods are not verified
  * by default (use --functions=... to override this). */
@ignore
class library      extends Annotation

/** Apply the "induct" tactic during verification of the annotated function. */
@ignore
class induct       extends Annotation

/** Only extract the contracts and replace the annotated function's body with a choose. */
@ignore
class extern       extends Annotation

/** Don't unfold the function's body during verification. */
@ignore
class opaque       extends Annotation

/** Specify that the annotated function is pure, which will be checked. */
@ignore
class pure         extends Annotation

/** Inline this function, but only once.
  * This might be useful if one wants to eg. inline a recursive function.
  * Note: A recursive function will not be inlined within itself. */
@ignore
class inlineOnce   extends Annotation

/** Instruct Stainless to partially evaluate calls to the annotated function. */
@ignore
class partialEval extends Annotation

/** Solidity annotation
**  A function declared payable can receive ether
**/
@ignore
class payable extends Annotation

/** Solidity annotation
**  A function declared payable can receive ether
**/
@ignore
class solidityPure extends Annotation

/** Solidity annotation
**  TODO
**/
@ignore
class view extends Annotation

/** Solidity annotation
**  TODO
**/
@ignore
class indexed extends Annotation

/** Solidity annotation
**  TODO
**/
@ignore
class solidityLibrary(name: String) extends Annotation


/**
 * Code annotated with @ghost is removed after stainless extraction.
 *
 * Code that can be annotated with @ghost: classes, method and value definitions, method parameters.
 *
 * See the Stainless specification for details.
 */
@ignore @field @getter @setter @param
class ghost extends StaticAnnotation
