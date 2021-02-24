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


object bundle_utils {

    def get_total_bundle_bits (that: Bundle) :Int = {
        var total :Int = 0

        for ((name, element) <- that.elements) {
            element match {
                case x: Bool        => total += 1
                case x: BitVector   => total += x.getBitsWidth
                case x: Bundle      => total += get_total_bundle_bits(x)
                case _              =>
            }
        }

        total
    }

    def bundle_to_bits (that :Bundle) : Bits = {
        val result = Bits (get_total_bundle_bits(that) bits)
        var index :Int = 0
        for ((name, element) <- that.elements) {
            element match {
                case x: Bool        => {
                    result (index) := x
                    index += 1
                }

                case x: BitVector   => {
                    result (index + x.getBitsWidth - 1 downto index) := x.asBits
                    index += x.getBitsWidth
                }

                case x: Bundle      =>
                    result(index + get_total_bundle_bits(x) - 1 downto index) := bundle_to_bits(x)
                    index += get_total_bundle_bits(x)

                case _              =>
            }
        }

        result
    }

    def bits_to_bundle (those_bits : Bits, that_bundle :Bundle) :Unit = {

        require (those_bits.getBitsWidth == get_total_bundle_bits(that_bundle))

        var index :Int = 0
        for ((name, element) <- that_bundle.elements) {
            element match {
                case x: Bool        => {
                    x :=  those_bits(index)
                    index += 1
                }

                case x: BitVector   => {
                    x := those_bits(index + x.getBitsWidth - 1 downto index)
                    index += x.getBitsWidth
                }

                case x: Bundle      => {
                    bits_to_bundle(those_bits(index + get_total_bundle_bits(x) - 1 downto index), x)
                    index += get_total_bundle_bits(x)
                }

                case _              =>
            }
        }
    }
}
