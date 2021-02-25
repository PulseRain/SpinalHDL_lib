/*
//=============================================================================
// Copyright (c) : Pulserain Technology, LLC. 2021 
//=============================================================================
//
// MIT License
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to 
// deal in the Software without restriction, including without limitation the 
// rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
// sell copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in 
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS 
// IN THE SOFTWARE.
*/


package common

import spinal.core._
import spinal.lib._


object file_utils {

    case class fileInput (val fileName : String, val numOfColumns :Int) {

        //println (s"Load input file ${fileName}")

        val lines: Iterator[String] = scala.io.Source.fromFile(fileName).getLines()
        val matrix: List[Array[Long]] = lines.map(_.split("\\s+").map(_.trim.toLong)).toList
        val rows: List[List[Long]] = matrix.map(_.take(numOfColumns).toList)

        var rowIndex :Int = 0

        def resetRowIndex(startValue :Int = 0) {
            rowIndex = startValue
        }

        def getNext() :List[Long] = {
            rowIndex += 1
            rows(rowIndex - 1)
        }
    }
    

    case class fileCompare (val fileName : String, val numOfColumns :Int, val cmpMask:Int, val cmpLength :Int, val cmpSkipCount :Int) {
        //println (s"compare file ${fileName}")

        val lines: Iterator[String] = scala.io.Source.fromFile(fileName).getLines()
        val matrix: List[Array[Long]] = lines.map(_.split("\\s+").map(_.trim.toLong)).toList
        val rows: List[List[Long]] = matrix.map(_.take(numOfColumns).toList)

        var cmpCount :Int = 0
        var misMatchedColumn :Int = 0
        var expectedValue :Long = 0
        var actualValue :Long = 0

        def resetCmpCount(startValue :Int = 0) {
            cmpCount = startValue
        }

        def matched (inputValue : List[Long]) : Boolean = {
            for (i <- 0 to numOfColumns - 1) {
                if (((cmpMask >> i) & 1) == 0) {
                    if (rows(cmpCount)(i) != inputValue(i)) {
                        misMatchedColumn = i
                        expectedValue = rows(cmpCount)(i)
                        actualValue   = inputValue(i)
                        return false
                    }
                }
            }

            cmpCount = cmpCount + 1
            true
        }

    }

}
