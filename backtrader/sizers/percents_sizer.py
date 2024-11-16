#!/usr/bin/env python
# -*- coding: utf-8; py-indent-offset:4 -*-
###############################################################################
#
# Copyright (C) 2015-2023 Daniel Rodriguez
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################
from __future__ import absolute_import, division, print_function, unicode_literals

import backtrader as bt

__all__ = ["PercentSizer", "AllInSizer", "PercentSizerInt", "AllInSizerInt"]


class PercentSizer(bt.Sizer):
    """This sizer return percents of available cash
      Этот сайзер возвращает процент от доступных денежных средств
      Параметры:
    - ``percents`` (по умолчанию: ``20``) - процент от доступных средств
      Params:
        - ``percents`` (default: ``20``)
    """

    params = (
        ("percents", 20),
        ("retint", False),  # return an int size or rather the float value
    )

    def __init__(self):
        pass

    def _getsizing(self, comminfo, cash, data, isbuy):
        position = self.broker.getposition(data)
        if not position:
            size = cash / data.close[0] * (self.params.percents / 100)
        else:
            size = position.size

        if self.p.retint:
            size = int(size)

        return size


class AllInSizer(PercentSizer):
    """This sizer return all available cash of broker
      Этот сайзер возвращает все доступные денежные средства брокера
      Параметры:
    - ``percents`` (по умолчанию: ``100``) - процент от доступных средств
       Params:
         - ``percents`` (default: ``100``)
    """

    params = (("percents", 100),)


class PercentSizerInt(PercentSizer):
    """This sizer return percents of available cash in form of size truncated
          to an int
          Этот сайзер возвращает процент от доступных денежных средств в виде размера,
      округленного до целого числа
      Параметры:
    - ``percents`` (по умолчанию: ``20``) - процент от доступных средств

          Params:
            - ``percents`` (default: ``20``)
    """

    params = (("retint", True),)  # return an int size or rather the float value


class AllInSizerInt(PercentSizerInt):
    """This sizer return all available cash of broker with the
          size truncated to an int
          Этот сайзер возвращает все доступные денежные средства брокера
      с размером, округленным до целого числа

           Params:
             - ``percents`` (default: ``100``)
          Параметры:
    - ``percents`` (по умолчанию: ``100``) - процент от доступных средств
    """

    params = (("percents", 100),)
